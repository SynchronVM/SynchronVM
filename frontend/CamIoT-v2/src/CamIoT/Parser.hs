{- | Parse a source file. -}
module CamIoT.Parser where

import qualified Data.Text as T
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Error as MP

import CamIoT.Internal.Syntax

import CamIoT.Parser.Preprocessor
import CamIoT.Parser.Parser

{- | Takes the contents of a source file and the file path of the source file and returns
either a pretty-printed @ParseErrorBundle@, or the successfully parsed program. -}

{- | Parse a program. The input is

  1. The contents of the source file
  2. The name of the source file

and the result if either an error or a successfully parsed program, where the program
is represented as a list of definitions. -}
parse :: T.Text -> FilePath -> Either String [Def ()]
parse source filepath =
    let preprocessed = preprocess source
    in mapLeft MP.errorBundlePretty $ MP.parse pProgram filepath preprocessed
  where
      -- | Map a function over the left component of a sum
      mapLeft :: (a -> c) -> Either a b -> Either c b
      mapLeft f (Left a) = undefined
      mapLeft _ (Right b) = Right b
