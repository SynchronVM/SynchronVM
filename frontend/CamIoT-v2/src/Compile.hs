module Compile where

import System.Exit

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as Map

import CamIoT.Internal.Syntax
import CamIoT.Rename
import CamIoT.Parser
import CamIoT.Typecheck
import CamIoT.Typecheck.Environment
import CamIoT.Typecheck.Substitution
import CamIoT.Pretty

-- * Compiling a program

-- | The actual compiler
compile :: FilePath -> IO ()
compile fp = do

    program                 <- parseProgram fp
    typechecked             <- typecheckProgram program
    (renamed, renamedstate) <- renameProgram typechecked
    putStrLn $ prettyProgram renamed

-- * Primitive, built in stuff

-- | A map that associates the primitive, built in identifiers with thier type schemas
primitiveIdentifiers :: Map.Map Ident Schema
primitiveIdentifiers = Map.fromList [{- Abis primitive functions -}]

-- | A map that associates the primitive, built in constructors with thier type schemas
primitiveConstructors :: Map.Map UIdent Schema
primitiveConstructors = Map.fromList [{- Abis primitive constructors -}]

-- * Parsing a program

{- | This function takes a filepath to a source program as input and tries to parse it.
If parsing fails, this function prints the parse error to standard out and then exits
the program with @exitFailure@. -}
parseProgram :: FilePath -> IO (Program ())
parseProgram fp = do
    -- read contents of source file
    source <- TIO.readFile fp

    -- try to parse
    defs <- case parse source fp of
        Left err -> do
            putStrLn err
            exitFailure
        Right defs -> return defs

    -- try to make [Def ()] into [Function ()]
    case mkProgram defs of
        Just p -> return p
        Nothing -> do
            putStrLn $ unlines [ "CamIoT.Compile.makeIntoProgram failed"
                               , "    The parsed definitions could not be turned into a program."
                               , "    This only happens if there is no main-function in the program."
                               ]
            exitFailure

-- * Typechecking a program

{- | Try to typecheck a program. If typechecking fails, this function will print the
error to standard out and then exit with @exitFailure@. Otherwise a type annotated
program is returned. -}
typecheckProgram :: Program () -> IO (Program Type)
typecheckProgram p = do
    -- typecheck the program with the built in typing environment
    case typecheck primitiveIdentifiers primitiveConstructors p of
        Left err -> do
            putStrLn err
            exitFailure
        Right (s,p) -> return $ apply s p -- apply substitution to annotated program

-- * Alpha renaming a program

{- | Alpha rename a program. If alphra renaming fails, this function will print the error
to standard out and then exit with @exitFailure@. Otherwise the alpha renamed program
is returned.

Note that there is only one case where this phase can fail, and failure can only be
caused by an error in the alpha renaming loop itself. If this is the case, smack Robert
on the fingers and put him to work fixing the bug. -}
renameProgram :: Program a -> IO (Program a, Int)
renameProgram p = do
    -- alpha rename, suppling a list of identifiers that should not be renamed
    case alphaRename doNotRename 0 p of
        Left err -> do
            putStrLn err
            exitFailure
        Right pi -> return pi
  where
      doNotRename :: [Ident]
      doNotRename = map fst $ Map.toList $ primitiveIdentifiers
