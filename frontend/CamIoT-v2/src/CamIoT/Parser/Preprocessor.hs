{- | Before a program can be parsed we need to do layout resolution. The programs we want
to parse are not context free, so without this stage compilation will not work.

@
f 5 = 5
f 7 = 3
f x = x
@

When the above program is turned into a token stream it becomes

@
f 5 = 5 f 7 = 3 f x = x
@

It is thus very hard to know where one equation end and the next begins. Layout
resolution will turn this into

@
f 5 = 5;
f 7 = 3;
f x = x;
@

which produces the following token stream

@
f 5 = 5 ; f 7 = 3 ; f x = x ;
@

making the start and stops of each equation explicit. This step is also applied to
case-expressions.

@
case ma of
    Just a -> a
    Nothing -> 3
@

becomes

@
case ma of {
    Just a -> a ;
    Nothing -> 3
}
@

-}
{-# LANGUAGE OverloadedStrings #-}
module CamIoT.Parser.Preprocessor where

import           Data.Maybe                     ( fromJust
                                                , isNothing
                                                )

import qualified Data.Text                     as T

import           CamIoT.Parser.Tokenize         ( Tok(TS)
                                                , TokPos
                                                , printTokPos
                                                , tokenize
                                                , toklength
                                                )

-- * Preprocessing

{- | Preprocess a source file by performing layout resolution. It is quite inefficient
to turn a source file @Text@ into a token stream just to turn it back into a
@Text@ later, but the parser can not operate on a token stream, so for now, let's do
it like this. -}
preprocess :: T.Text -> T.Text
preprocess source = printTokPos $ resolveLayout topLayout $ tokenize source

-- * Layout resolution

{- | This bool says that we definitely want to apply top layout. This means that
top level definitions are delimited by semi-colons. -}
topLayout :: Bool
topLayout = True

-- | These words initiate/terminate layout blocks
layoutWords, layoutStopWords :: [T.Text]
layoutWords = ["where", "of"]
layoutStopWords = []

layoutOpen, layoutClose, layoutSep :: T.Text
layoutOpen = "{"
layoutClose = "}"
layoutSep = ";"

-- | Replace layout syntax with explicit layout tokens.
resolveLayout
  :: Bool    -- ^ Whether to use top-level layout.
  -> [TokPos]
  -> [TokPos]
resolveLayout tp = res Nothing [if tl then Implicit 1 else Explicit] [0]
 where
  -- Do top-level layout if the function parameter and the grammar say so.
  tl = tp && topLayout

  res
    :: Maybe TokPos -- ^ The previous token, if any.
    -> [Block] -- ^ A stack of layout blocks.
    -> [Int]
    -> [TokPos]
    -> [TokPos]

  -- The stack should never be empty.
  res _ [] _ ts = error $ "Layout error: stack empty. Tokens: " ++ show ts

  res _ st c (t0 : ts) |
    -- We found an open brace in the input,
    -- put an explicit layout block on the stack.
    -- This is done even if there was no layout word,
    -- to keep opening and closing braces.
                         isLayoutOpen t0 =
    moveAlong (Explicit : st) [t0] ts c

  -- We are in an implicit layout block
  res pt st@(Implicit n : ns) c (t0 : ts)
    |

      -- End of implicit block by a layout stop word
      isStop t0
    =
           -- Exit the current block and all implicit blocks
           -- more indented than the current token
      let (ebs, ns') = span (`moreIndent` column t0) ns
          moreIndent (Implicit x) y = x > y
          moreIndent Explicit     _ = False
          -- the number of blocks exited
          b          = 1 + length ebs
          bs         = replicate b layoutClose
          -- Insert closing braces after the previous token.
          (ts1, ts2) = splitAt (1 + b) $ addTokens (afterPrev pt) bs (t0 : ts)
      in  moveAlong ns' ts1 ts2 (drop b c)
    |

    -- End of an implicit layout block
      newLine pt t0 && column t0 < n
    =
           -- Insert a closing brace after the previous token.
      let b : t0' : ts' = addToken (afterPrev pt) layoutClose (t0 : ts)
                                                                  -- Repeat, with the current block removed from the stack
      in  moveAlong ns [b] (t0' : ts') $ tail c
    |

    -- Encounted a new line in an implicit layout block, and that new line
    -- character was a parenthesis.
      isParenthesesOpen t0 && newLine pt t0 && column t0 == n
    =
       -- Insert a semicolon after the previous token.
       -- unless we are the beginning of the file,
       -- or the previous token is a semicolon or open brace.
      if isNothing pt || isTokenIn [layoutSep, layoutOpen] (fromJust pt)
      then moveAlong st [t0] ts c
      else
        let b : t0' : ts' = addToken (afterPrev pt) layoutSep (t0 : ts)
        in  moveAlong st [b, t0'] ts' $ incOpening c
    |

    -- see opening parentheses
      isParenthesesOpen t0
    = moveAlong st [t0] ts $ incOpening c
    | isParenthesesClose t0
    = if head c == 0
      then
        let b : t0' : ts' = addToken (afterPrev pt) layoutClose (t0 : ts)
        in  moveAlong ns [b] (t0' : ts') $ tail c
      else moveAlong st [t0] ts $ decOpening c

  res pt st c (t0 : ts)
    |
    -- Start a new layout block if the first token is a layout word
      isLayout t0
    = case ts of
            -- Explicit layout, just move on. The case above
            -- will push an explicit layout block.
      t1 : _ | isLayoutOpen t1 -> moveAlong st [t0] ts c
           -- The column of the next token determines the starting column
           -- of the implicit layout block.
           -- However, the next block needs to be strictly more indented
           -- than the previous block.
      _ ->
        let
          col = max (indentation st + 1)
            $
            -- at end of file, the start column doesn't matter
              if null ts then column t0 else column (head ts)
          -- insert an open brace after the layout word
          b : ts' = addToken (nextPos t0) layoutOpen ts
          -- save the start column
          st'     = Implicit col : st
        in -- Do we have to insert an extra layoutSep?
          case st of
            Implicit n : _
              | newLine pt t0
                && column t0
                == n
                && not
                     (  isNothing pt
                     || isTokenIn [layoutSep, layoutOpen] (fromJust pt)
                     )
              -> let b' : t0' : b'' : ts'' =
                       addToken (afterPrev pt) layoutSep (t0 : b : ts')
                 in  moveAlong st' [b', t0', b''] ts' (0 : c)
            _ -> moveAlong st' [t0, b] ts' (0 : c)
    |

    -- If we encounter a closing brace, exit the first explicit layout block.
      isLayoutClose t0
    = let tod = dropWhile isImplicit st
          st' = drop 1 tod
          c'  = drop (length c - length tod) c
      in  if null st'
            then
              error
              $  "Layout error: Found "
              ++ (T.unpack layoutClose)
              ++ " at ("
              ++ show (line t0)
              ++ ","
              ++ show (column t0)
              ++ ") without an explicit layout block."
            else moveAlong st' [t0] ts c'

  -- Insert separator if necessary.
  res pt st@(Implicit n : ns) c (t0 : ts) |
    -- Encounted a new line in an implicit layout block.
                                            newLine pt t0 && column t0 == n =
       -- Insert a semicolon after the previous token.
       -- unless we are the beginning of the file,
       -- or the previous token is a semicolon or open brace.
    if isNothing pt || isTokenIn [layoutSep, layoutOpen] (fromJust pt)
      then moveAlong st [t0] ts c
      else
        let b : t0' : ts' = addToken (afterPrev pt) layoutSep (t0 : ts)
        in  moveAlong st [b, t0'] ts' c

  -- Nothing to see here, move along.
  res _ st c (t : ts) = moveAlong st [t] ts c

  -- At EOF: skip explicit blocks.
  res (Just t) (Explicit : bs) c [] | null bs   = []
                                    | otherwise = res (Just t) bs c []

  -- If we are using top-level layout, insert a semicolon after
  -- the last token, if there isn't one already
  res (Just t) [Implicit _n] _ []
    | isTokenIn [layoutSep] t = []
    | otherwise               = addToken (nextPos t) layoutSep []

  -- At EOF in an implicit, non-top-level block: close the block
  res (Just t) (Implicit _n : bs) (_ : co) [] =
    let c = addToken (nextPos t) layoutClose [] in moveAlong bs c [] co

  -- This should only happen if the input is empty.
  res Nothing _st _ [] = []

  -- | Move on to the next token.
  moveAlong
    :: [Block] -- ^ The layout stack.
    -> [TokPos] -- ^ Any tokens just processed.
    -> [TokPos] -- ^ the rest of the tokens.
    -> [Int]   -- ^ Opening counts
    -> [TokPos]
  moveAlong _  [] _  _ = error "Layout error: moveAlong got [] as old tokens"
  moveAlong st ot ts c = ot ++ res (Just $ last ot) st c ts

  newLine :: Maybe TokPos -> TokPos -> Bool
  newLine pt t0 = case pt of
    Nothing -> True
    Just t  -> line t /= line t0

data Block
   = Implicit Int -- ^ An implicit layout block with its start column.
   | Explicit
   deriving Show

-- | Get current indentation.  0 if we are in an explicit block.
indentation :: [Block] -> Int
indentation (Implicit n : _) = n
indentation _                = 0

-- | Check if s block is implicit.
isImplicit :: Block -> Bool
isImplicit (Implicit _) = True
isImplicit _            = False

type Position = (Int, Int)

-- | Insert a number of tokens at the begninning of a list of tokens.
addTokens
  :: Position -- ^ Position of the first new token.
  -> [T.Text] -- ^ Token symbols.
  -> [TokPos]  -- ^ The rest of the tokens. These will have their
                      --   positions updated to make room for the new tokens .
  -> [TokPos]
addTokens p ss ts = foldr (addToken p) ts ss

-- | Insert a new symbol token at the begninning of a list of tokens.
addToken
  :: Position -- ^ Position of the new token.
  -> T.Text   -- ^ Symbol in the new token.
  -> [TokPos]  -- ^ The rest of the tokens. These will have their
                     --   positions updated to make room for the new token.
  -> [TokPos]
addToken p s ts = sToken p s : map (incrGlobal p (T.length s)) ts

-- | Get the position immediately to the right of the given token.
--   If no token is given, gets the first position in the file.
afterPrev :: Maybe TokPos -> Position
afterPrev = maybe (1, 1) nextPos

-- | Get the position immediately to the right of the given token.
nextPos :: TokPos -> Position
nextPos (t, l, c) = (l, c + s + 1) --    Pn (g + s) l (c + s + 1)
  where s = toklength t

-- | Add to the global and column positions of a token.
--   The column position is only changed if the token is on
--   the same line as the given position.
incrGlobal
  :: Position -- ^ If the token is on the same line
                       --   as this position, update the column position.
  -> Int      -- ^ Number of characters to add to the position.
  -> TokPos
  -> TokPos
incrGlobal (l0, _) i (t, l, c) = --(PT (Pn g l c) t) =
                                 if l /= l0
  then (t, l, c) --    PT (Pn (g + i) l c) t
  else (t, l, c + i) --    PT (Pn (g + i) l (c + i)) t
--incrGlobal _ _ p = error $ "cannot add token at " ++ show p

-- | Create a symbol token.
sToken :: Position -> T.Text -> TokPos
sToken (l, c) s = (TS s i, l, c)
 where
  i = case s of
    "!"     -> 1
    "&&"    -> 2
    "("     -> 3
    "()"    -> 4
    ")"     -> 5
    "*"     -> 6
    "+"     -> 7
    ","     -> 8
    "-"     -> 9
    "->"    -> 10
    "/"     -> 11
    ":"     -> 12
    ";"     -> 13
    "<"     -> 14
    "="     -> 15
    "=="    -> 16
    ">"     -> 17
    "Bool"  -> 18
    "False" -> 19
    "Float" -> 20
    "Int"   -> 21
    "True"  -> 22
    "\\"    -> 23
    "_"     -> 24
    "as"    -> 25
    "case"  -> 26
    "data"  -> 27
    "else"  -> 28
    "if"    -> 29
    "in"    -> 30
    "let"   -> 31
    "of"    -> 32
    "then"  -> 33
    "where" -> 34
    "{"     -> 35
    "||"    -> 36
    "}"     -> 37
    "<="    -> 38
    ">="    -> 39
    _       -> error $ "not a reserved word: " ++ show s

-- | Get the position of a token.
position :: TokPos -> Position
position (_, l, c) = (l, c)

-- | Get the line number of a token.
line :: TokPos -> Int
line t = case position t of
  (l, _) -> l

-- | Get the column number of a token.
column :: TokPos -> Int
column t = case position t of
  (_, c) -> c

-- | Check if a token is one of the given symbols.
isTokenIn :: [T.Text] -> TokPos -> Bool
isTokenIn ts t = case t of
  (TS r _, _, _) | r `elem` ts -> True
  _                            -> False

-- | Check if a word is a layout start token.
isLayout :: TokPos -> Bool
isLayout = isTokenIn layoutWords

-- | Check if a token is a layout stop token.
isStop :: TokPos -> Bool
isStop = isTokenIn layoutStopWords

-- | Check if a token is the layout open token.
isLayoutOpen :: TokPos -> Bool
isLayoutOpen = isTokenIn [layoutOpen]

-- | Check if a token is the layout close token.
isLayoutClose :: TokPos -> Bool
isLayoutClose = isTokenIn [layoutClose]

isParenthesesOpen :: TokPos -> Bool
isParenthesesOpen = isTokenIn ["("]

isParenthesesClose :: TokPos -> Bool
isParenthesesClose = isTokenIn [")"]

incOpening :: [Int] -> [Int]
incOpening (x : xs) = (x + 1 : xs)
incOpening [] = error "tried to increment opening count on empty count stack"

decOpening :: [Int] -> [Int]
decOpening (x : xs) = (x - 1 : xs)
