{- | This module exports functionality that pretty prints camiot programs. The programs
are pretty printed to a format that is almost identical to what the source program looked
like. There are some extra whitespaces inserted in places, and some whitespaces are
removed in other places. -}
module CamIoT.Pretty where

import           CamIoT.Internal.Syntax         ( Program )
import           CamIoT.Pretty.Syntax           ( Print
                                                , printTree
                                                )

-- | Pretty print a camiot program
prettyProgram :: Print a => Program a -> String
prettyProgram = printTree
