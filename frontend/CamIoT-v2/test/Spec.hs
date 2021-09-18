import           Data.Either
import qualified Data.Map                      as Map

import           Test.QuickCheck
import           Generator

import           CamIoT.Internal.Syntax hiding (main)
import           CamIoT.Typecheck

main :: IO ()
main = quickCheck $ withMaxSuccess 10000 typecheckProperty

typecheckProperty :: Program () -> Bool
typecheckProperty p = isRight $ typecheck Map.empty Map.empty p
