{- | This module implements the parser that parses the source file as a @Text@ and
returns an object of the internal AST. The parser is hand written using the package
MegaParsec.

Parsing a program is a bit tricky, but not by much. Getting the precedences right needs
some care.
The strategy to use is that if e.g a parser such as `pExpAdd` needs to recursively parse
two operand expressions, these recursive expressions should be parsed with the expression
parser of precedence level one higher than that of the `pExpAdd` parser. The parser in
this file are listed in order of precedence. E.g for types

  1. `pClosed` \'closed\' expressions of infinite precedence. Will recursively call the
      parser of lowest precedence, `pFun`.
  2. `pApp` next higher precedence level, will perform recursive calls to `pClosed`
  3. `pFun` weakest precedence level, will perform recursive calls to `pApp`
  4. `pType` consumes leading whitespace and calls `pFun`
-}
{-# LANGUAGE OverloadedStrings #-}
module CamIoT.Parser.Parser where

import           Data.Functor
import qualified Data.Text                     as T
import           Data.Void

import           CamIoT.Internal.Syntax
import           CamIoT.Parser.Keywords

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as Lexer

-- | Custom parser type - synonym for Parsec Void Text a
type Parser a = Parsec Void T.Text a

-- * Parse a program

-- | Top level parser that parses a program
pProgram :: Parser [Def ()]
pProgram = many $ pDataDec <|> try pTypeSignature <|> pEquation

-- * Parse strategies for language constructs

-- ** Parse Types

-- | A parser of closed types, aka types with infinite presedence.
pClosed :: Parser Type
pClosed = choice
     [ TInt <$ pSymbol "Int"
     , TFloat <$ pSymbol "Float"
     , TBool <$ pSymbol "Bool"
     , TVar <$> pIdent
     , flip TAdt [] <$> pUIdent
     , do
          pChar '('
          ts <- sepBy pFun (pChar ',') <* pChar ')'
          case ts of
               []          -> pure TNil
               [t        ] -> pure t
               (_ : _ : _) -> pure (TTup ts)
     ]

{- | Parser that parses an algebraic data type (An uppercase constructor followed by
zero or more types). -}
pApp :: Parser Type
pApp = choice [TAdt <$> pUIdent <*> many pClosed, pClosed]

-- | Parser that parses function types
pFun :: Parser Type
pFun = foldr1 TLam <$> sepBy1 pApp (pSymbol "->")

-- | A parser that parses a type
pType :: Parser Type
pType = pSpace *> pFun

-- ** Parse Expressions

-- | A parser that parses closed expressions, aka expressions of infinite presedence
pExpClosed :: Parser (Exp ())
pExpClosed = choice
     [ ELit () <$> pConst
     , EVar () <$> pIdent
     , ECon () <$> pUIdent
     , do
          pChar '('
          es <- sepBy1 pExpVerbose (pChar ',') <* pChar ')'
          case es of
               []          -> undefined
               [e        ] -> pure e
               (_ : _ : _) -> pure (ETup () es)
     ]

-- | Parser that parses function applications
pExpApp :: Parser (Exp ())
pExpApp = foldl1 (EApp ()) <$> some pExpClosed

-- | Parser that parses boolean negation expressions
pExpNot :: Parser (Exp ())
pExpNot = choice
     [ do
          pChar '!'
          e <- pExpApp
          pure $ EUn () e (Not ())
     , pExpApp
     ]

-- | Parser that parses multiplication or divisions of expressions
pExpMul :: Parser (Exp ())
pExpMul = pExpNot >>= go  where
    go e1 = choice
         [ do
              pChar '*'
              e2 <- pExpNot
              go $ EBin () e1 e2 (Mul ())
         , do
              pChar '/'
              e2 <- pExpNot
              go $ EBin () e1 e2 (Div ())
         , pure e1
         ]

-- | Parser that parses addition or subtraction of expressions
pExpAdd :: Parser (Exp ())
pExpAdd = pExpMul >>= go  where
    go e1 = choice
         [ do
              pChar '+'
              e2 <- pExpMul
              go $ EBin () e1 e2 (Add ())
         , do
              pChar '-'
              e2 <- pExpMul
              go $ EBin () e1 e2 (Sub ())
         , pure e1
         ]

-- | Parser that parses relational binary operations on expressions
pExpRel :: Parser (Exp ())
pExpRel = pExpAdd >>= go  where
    go e1 = choice
         [ do
              pChar '<'
              choice
                   [ do
                        pChar '='
                        e2 <- pExpAdd
                        pure $ EBin () e1 e2 (OLE ())
                   , do
                        e2 <- pExpAdd
                        pure $ EBin () e1 e2 (OLT ())
                   ]
         , do
              pChar '>'
              choice
                   [ do
                        pChar '='
                        e2 <- pExpAdd
                        pure $ EBin () e1 e2 (OGE ())
                   , do
                        e2 <- pExpAdd
                        pure $ EBin () e1 e2 (OGT ())
                   ]
         , do
              pSymbol "=="
              e2 <- pExpAdd
              pure $ EBin () e1 e2 (OEQ ())
         , pure e1
         ]

-- | Parser that parses boolean conjunction of expressions
pExpAnd :: Parser (Exp ())
pExpAnd =
     foldr1 (\e1 e2 -> EBin () e1 e2 (And ())) <$> sepBy1 pExpRel (pSymbol "&&")

-- | Parser that parses boolean disjunction of expressions
pExpOr :: Parser (Exp ())
pExpOr =
     foldr1 (\e1 e2 -> EBin () e1 e2 (Or ())) <$> sepBy1 pExpAnd (pSymbol "||")

-- | Parser that parses verbose expressions. These expressions has the lowest presedence.
pExpVerbose :: Parser (Exp ())
pExpVerbose = choice
     [ do
          pSymbol "let"
          p <- pPat False False
          pSymbol "="
          e1 <- pExpVerbose
          pSymbol "in"
          ELet () p e1 <$> pExpVerbose
     , do
          pChar '\\'
          p <- pPat False False
          pSymbol "->"
          ELam () p <$> pExpVerbose
     , do
          pSymbol "if"
          e1 <- pExpVerbose
          pSymbol "then"
          e2 <- pExpVerbose
          pSymbol "else"
          e3 <- pExpVerbose
          return $ EIf () e1 e2 e3
     , do
          pSymbol "case"
          e <- pExpVerbose
          pSymbol "of"
          pChar '{'
          branches <- sepBy1
               (do
                    p <- pPat True True
                    pSymbol "->"
                    e <- pExpVerbose
                    return (p, e)
               )
               (pChar ';')
          pChar '}'
          return $ ECase () e branches
     , pExpOr
     ]

-- | A parser that parses an expressions
pExp :: Parser (Exp ())
pExp = pSpace *> pExpVerbose

-- ** Parse Definitions

-- | A parser that parses a type signature
pTypeSignature :: Parser (Def ())
pTypeSignature = do
     name <- pIdent
     pChar ':'
     t <- pType
     pChar ';'
     return $ DTypeSig name t

-- | A parser that parses a datatype declaration
pDataDec :: Parser (Def ())
pDataDec = do
     pSymbol "data"
     uid  <- pUIdent
     vars <- many pIdent
     pSymbol "where"
     pChar '{'
     constructors <- sepBy
          (do
               con <- pUIdent
               pChar ':'
               typ <- pType
               return (con, typ)
          )
          (pChar ';')
     pChar '}'
     pChar ';'
     return $ DDataDec uid vars constructors

-- | A parser that parses a function definition
pEquation :: Parser (Def ())
pEquation = do
     name     <- pIdent
     patterns <- many (pPat True False)
     pSymbol "="
     exp <- pExp
     pChar ';'
     return $ DEquation () name patterns exp

-- ** Parse Patterns

{- | A parser that parses a closed pattern. The first boolean specifies wether constants
are allowed in the pattern. E.g should we allow @\5 -> 2@? Probably yes, as this should
be rejected by the type checker (if at all). The second boolean specifies wether we allow
N-ary data constructors to be parsed. If they are not, we must wrap them in parentheses.
This is because for case clauses we'd like to be allowed to omit the parentheses around
the @Just@ case in this example:

@
case x of
  Just x  -> undefined
  Nothing -> undefined
@

In this case the second boolean would be @True@. E.g when we are parsing a function
definition, however, the boolean is @False@ as it will parse the wrong thing.

This is wrong since @Just@ has arity 1
@
f Just x Nothing = undefined
@

While this is parsed correctly.
@
f (Just x) Nothing = undefined
@ -}
pPatClosed :: Bool -> Bool -> Parser (Pat ())
pPatClosed allowConstants allowNary = choice $ maybe ++ always
  where
    maybe = [ PConst () <$> pConst | allowConstants ]
    always =
         [ PVar () <$> pIdent
         , flip (PAdt ()) [] <$> pUIdent
         , PWild () <$ pChar '_'
         , do
              pChar '('
              ps <- sepBy (pPatAs allowConstants allowNary) (pChar ',')
                   <* pChar ')'
              case ps of
                   []          -> pure $ PNil ()
                   [p        ] -> pure p
                   (_ : _ : _) -> pure (PTup () ps)
         ]

-- | A parses that parse patterns of data constructors
pPatApp :: Bool -> Bool -> Parser (Pat ())
pPatApp allowconstants allowNary =
     choice $ pAdt ++ [pPatClosed allowconstants allowNary]
  where
    pAdt = if allowNary
         then [adt]
         else [try $ parens adt, pPatClosed allowconstants allowNary]
    adt = do
         con  <- pUIdent
         vars <- many (pPatClosed allowconstants allowNary)
         return $ PAdt () con vars

-- | A parser that parses as-patterns
pPatAs :: Bool -> Bool -> Parser (Pat ())
pPatAs allowConstants allowNary = choice
     [ try $ do
          x <- pIdent
          pSymbol "as"
          p <- pPatAs allowConstants allowNary
          return $ PAs () x p
     , pPatApp allowConstants allowNary
     ]

-- | A parser that parses a single pattern
pPat :: Bool -> Bool -> Parser (Pat ())
pPat allowConstants allowNary = pSpace *> pPatAs allowConstants allowNary

-- ** Parse Constants

-- | A parser that parses a constant literal
pConst :: Parser Lit
pConst = choice
     [ try $ LFloat <$> Lexer.lexeme pSpace Lexer.float
     , LInt <$> Lexer.lexeme pSpace Lexer.decimal
     , LBool <$> ((True <$ pSymbol "True") <|> (False <$ pSymbol "False"))
     , LNil <$ pSymbol "()"
     ]

-- ** Parser utilities

{- | A parser that takes a parser as input and produces a parser that behaves like the
input parser but which requires the parsed item to be wrapped in parentheses. -}
parens :: Parser a -> Parser a
parens p = label "parse a type wrapped in parentheses" $ do
     pSymbol "("
     a <- p
     pSymbol ")"
     return a

-- | A parser of identifiers. Will fail if the identifier is a keyword.
pIdent :: Parser Ident
pIdent = try $ do
     a         <- lowerChar
     rest      <- many $ choice [letterChar, digitChar, char '_']
     trailings <- many (char '\'')
     pSpace
     let x = a : (rest ++ trailings)
     if x `elem` keywords
          then fail "found keyword, expected identifier"
          else return $ Ident x

-- | A parser of uppercase identifiers. Will fail if the identifier is a keyword.
pUIdent :: Parser UIdent
pUIdent = try $ do
     a    <- upperChar
     rest <- many $ choice [letterChar, digitChar, char '_']
     pSpace
     let x = a : rest
     if x `elem` keywords
          then fail "found keyword, expected uppercase identifier"
          else pure $ UIdent x

-- | A parser of symbols
pSymbol :: T.Text -> Parser T.Text
pSymbol = Lexer.symbol pSpace

-- | A parser of characters
pChar :: Char -> Parser ()
pChar c = void (char c <* pSpace)

-- | A parser that consumes whitespace and some other fluff
pSpace :: Parser ()
pSpace = Lexer.space (void spaceChar)
                     (Lexer.skipLineComment "--")
                     (Lexer.skipBlockComment "{-" "-}")
