-- MIT License

-- Copyright (c) 2020 Robert Krook

-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:

-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.
{-# LANGUAGE OverloadedStrings #-}
module Parser.Preprocessor (process) where

import Control.Monad.State.Lazy
    ( when, gets, modify, MonadState(get), StateT(runStateT) )
import Control.Monad.Writer
    ( when, runWriter, MonadWriter(tell), Writer )
import Control.Monad.Except ( when )
import qualified Data.Text as T


data PPState = ST 
     { source       :: T.Text  -- ^ The contents that remains unpreprocessed so far
     , current      :: Int     -- ^ The column number of the last fetched token
     , targetIndent :: [Int]}  -- ^ A stack of stored columns
  deriving Show

data Error = IndentationError Int Int String
instance Show Error where
    show (IndentationError expected found token) =
        "Parse error: expected indentation " ++ show expected ++ 
        " of token " ++ token ++ " but found " ++ show found

type PP a = StateT PPState (Writer T.Text) a

keywords :: [T.Text]
keywords = ["where", "of"]

-- | Function that will take a `Text` and preprocess it
process :: T.Text -> T.Text
process t =
    let wr = runStateT process_ (ST t 0 [0])
        ((_,_),w) = runWriter wr
        w' = T.dropWhile (/= ';') w
    in T.snoc (T.tail w') ';'

process_ :: PP ()
process_ = do
    (t,i) <- nextToken
    if t /= T.empty
        then (do
            while (getCurrentTarget >>= \i' -> if t `T.isPrefixOf` "--" ||
                                                  t `T.isPrefixOf` "{-" ||
                                                  t `T.isPrefixOf` "-}"
                                               then return False
                                               else emitFluff i i')
            tell t
            when (isKeyword t) (do
                tell "{"
                (t',i') <- nextToken
                pushCurrentTarget i'
                tell t')
            process_)
        else while (do
            target <- getCurrentTarget
            if target > 0
                then tell "}" >> popCurrentTarget >> return True
                else return False)

emitFluff :: Int -> Int -> PP Bool
emitFluff i1 i2
  | i1 < i2  = tell "}" >> popCurrentTarget >> return True
  | i1 == i2 = tell ";" >> return False
  | i1 > i2  = return False

getCurrentTarget :: PP Int
getCurrentTarget = head <$> gets targetIndent

popCurrentTarget :: PP ()
popCurrentTarget = modify $ \(ST r c (_:ts)) -> ST r c ts

pushCurrentTarget :: Int -> PP ()
pushCurrentTarget i = modify $ \(ST r c ts) -> ST r c (i:ts)

isKeyword :: T.Text -> Bool
isKeyword token = token `elem` keywords

nextToken :: PP (T.Text, Int)
nextToken = do
    (ST t i ti) <- get

    -- first split the input up in its leading whitespace/newline and the rest
    let (fluff, t')  = T.span (\c -> c == ' ' || c == '\n') t
    -- then extract the next token and the rest of the input
    let (token, t'') = T.span (\c -> c /= ' ' && c /= '\n') t'


    -- update the internal column counter
    updateColumn fluff
    c <- gets current

    -- emit the things we just cut off, to keep
    -- the annotated code as close to the source as possible
    tell fluff

    -- modify the internal state to reflect the rest of the input
    modify $ \(ST _ c i) -> ST t'' c i

    -- get the column of the current token and return the subsequent pair
    column <- gets current
    return (token, column)

updateColumn :: T.Text -> PP ()
updateColumn t = do
    -- is there a newline?
    -- if no symbol matches span will not 'consume' anything
    let (spaces, rest) = T.span (== '\n') t
    if spaces == T.empty
        -- if there wasn't, we count the number of spaces and add that to c
        then modify $ \(ST t' c i) -> ST t' (c + T.length rest) i
        -- otherwise we've encountered a newline and we set the internal counter
        -- to c and continue with a recursive call
        else modify (\(ST t' _ i)  -> ST t' 0 i) >> updateColumn rest

-- while mb returns true, execute ma
while :: Monad m => m Bool -> m ()
while mb = do
    b <- mb
    when b (while mb)