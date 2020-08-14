module AstUtils(
    getExpvar
  , getTypvar  

  , tupExp
  , deTupExp
  , tupType
  , deTupType
  , deTupPat
  , deAdtPat

  , function_type
  , unwrap_function
  , count_arguments

  , (*->)
  , bool
  , int
  , float
) where

import AbsTinyCamiot

-- TODO make heavy use of this bad boy.... it would make
-- the inference code easier to read, I believe
infixr 8 *->
(*->) :: Type () -> Type () -> Type ()
t1 *-> t2 = TLam () t1 t2

getExpvar :: Exp a -> a
getExpvar e = case e of
    ETup a _      -> a
    ECase a _ _   -> a
    ELet a _ _ _  -> a
    ELetR a _ _ _ -> a
    ELam a _ _    -> a
    EIf a _ _ _   -> a
    EApp a _ _    -> a
    EOr a _ _     -> a
    EAnd a _ _    -> a
    ERel a _ _ _  -> a
    EAdd a _ _ _  -> a
    EMul a _ _ _  -> a
    ENot a _      -> a
    EVar a _      -> a
    EConst a _    -> a

getTypvar :: Type a -> a
getTypvar (TLam a _ _) = a
getTypvar (TTup a _)   = a
getTypvar (TNil a)     = a
getTypvar (TVar a _)   = a
getTypvar (TAdt a _ _) = a
getTypvar (TInt a)     = a
getTypvar (TFloat a)   = a
getTypvar (TBool a)    = a

{- Some conversion to and from closely related types -}
tupExp :: Exp a -> TupExp a
tupExp e = ETupExp (getExpvar e) e

deTupExp :: TupExp a -> Exp a
deTupExp (ETupExp _ e) = e

tupType :: Type a -> TupType a
tupType t = TTupType (getTypvar t) t

deTupType :: TupType a -> Type a
deTupType (TTupType _ t) = t

deTupPat :: TupPat a -> Pat a
deTupPat (PTupPat _ p) = p

deAdtPat :: AdtPat a -> Pat a
deAdtPat (PAdtPat _ p) = p

-- builds a function type from the list of argument types and the result type
function_type :: [Type ()] -> Type () -> Type ()
function_type [] res     = res
function_type (x:xs) res = TLam () x (function_type xs res)

-- fetch the final construction of a type
-- e.g unwrap function (a -> b -> Either a b) = Either a b
unwrap_function :: Type () -> Type ()
unwrap_function (TLam () _ t) = unwrap_function t
unwrap_function t             = t

count_arguments :: Type () -> Int
count_arguments (TLam () _ t) = 1 + count_arguments t
count_arguments _             = 0

-- synonyms for the built-in types
bool :: Type ()
bool = TAdt () (UIdent "Bool") []

int :: Type ()
int = TAdt () (UIdent "Int") []

float :: Type ()
float = TAdt () (UIdent "Float") []