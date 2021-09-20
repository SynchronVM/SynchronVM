{- | This module exports a single entity `keywords`, which is a list containing all the
keywords/reserved words in the language. It is not allowed for identifiers to assume
any of these names. -}
module CamIoT.Parser.Keywords where

-- | Reserved words in the language
keywords :: [String]
keywords =
  [
  -- types
    "Bool"
  , "Int"
  , "Float"

  -- constants
  , "True"
  , "False"

  -- misc
  , "data"
  , "where"
  , "case"
  , "of"
  , "let"
  , "in"
  , "if"
  , "then"
  , "else"
  , "as"
  ]
