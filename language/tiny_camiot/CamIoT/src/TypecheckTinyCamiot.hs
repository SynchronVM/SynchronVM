module TypecheckTinyCamiot where

import AbsTinyCamiot

typecheck :: [Def ()] -> Either String [Def ()]
typecheck = return