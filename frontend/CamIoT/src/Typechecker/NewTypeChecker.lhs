\documentclass{article}

%include lhs2TeX.fmt
%include lhs2TeX.sty
%include spacing.fmt

\long\def\ignore#1{}

\begin{document}

Let's write a type checker (again) without errors! Famous last words. Lots of this is based on
Stephen Diehls tutorial from his blog.
\ignore{
\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Typechecker.NewTypeChecker where

import Data.Maybe
import Data.Foldable
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

import System.IO.Unsafe

trace :: Show a => a -> a
trace x = unsafePerformIO $ putStrLn (show x) >> return x
\end{code}
}

First of all, to typecheck programs we need to give our language types. Let's decide that we only allow
type variables, integers, pairs and function types.
\begin{code}
data Type = TVar Ident | TInt | TPair Type Type | TLam Type Type
  deriving (Eq, Show)
\end{code}

We need to identify elements in our language, such as variable names and function names.
\begin{code}
data Ident = Ident String deriving (Eq, Show, Ord)
\end{code}

A nice functional language should let us use pattern matching and other nice features, so let's add some
patterns. In this small language we will just allow variables, constants and pairs.
\begin{code}
data Pat a = PVar a Ident
           | PConst a Lit
           | PTup a (Pat a) (Pat a)
  deriving (Eq, Show)

patVar :: Pat a -> a
patVar p = case p of
  PVar a _   -> a
  PConst a _ -> a
  PTup a _ _ -> a
\end{code}
\begin{code}
data Lit = LInt Int
  deriving (Eq, Show)
\end{code}

A functional program is really just a large expression. The expressions we will allow in this language are
variables, literals, addition of expressions, lambda abstractions, function application and let-bindings.
\begin{code}
data Exp a = EVar a Ident
           | ELit a Lit
           | EAdd a (Exp a) (Exp a)
           | ELam a (Pat a) (Exp a)
           | EApp a (Exp a) (Exp a)
           | ELet a (Pat a) (Exp a) (Exp a)
  deriving (Eq, Show)

expVar :: Exp a -> a
expVar e = case e of
  EVar a _     -> a
  ELit a _     -> a
  EAdd a _ _   -> a
  ELam a _ _   -> a
  ELet a _ _ _ -> a
\end{code}

The top level definitions are either type signatures or equations. Note here that we can have many
equations that bind an expression to the same name. Examples are
\begin{verbatim}
f a = 3
f 3 = 5
\end{verbatim}
We have an invariant that says that \textit{if} there exists a type signature for a function, that
signature precedes the function definitions in the list of definitions. We also define a helper function
to retrieve the name of a definition.
\begin{code}
data Def a = DTypeSig Ident Type
           | DEquation a Ident [Pat a] (Exp a)
  deriving (Eq, Show)

getName :: Def a -> Ident
getName (DTypeSig id _)      = id
getName (DEquation _ id _ _) = id

defVar :: Def a -> Maybe a
defVar d = case d of
  DTypeSig _ _      -> Nothing
  DEquation a _ _ _ -> Just a
\end{code}

When our parser is done and we have a \verb|[Def ()]| the first thing we want to do is to convert it
to a \verb|[Function ()]| instead. This let's us 'clump together' many \verb|Def ()| that constitutes
equations for the same function.
\begin{code}
data Function a = Function
  { name      :: Ident
  , equations :: [Def a]
  , typesig   :: Maybe Type
  }
  deriving (Eq, Show)

-- Use a forall to make the type of `groups` use the same `a`
mkFunctions :: forall a . [Def a] -> [Function a]
mkFunctions defs = map toFunction $ groups defs

pred :: Def a -> Def a -> Bool
pred d1 d2 = getName d1 == getName d2

groups :: [Def a] -> [[Def a]]
groups defs = groupBy Typechecker.NewTypeChecker.pred defs

-- assuming a singleton list is a DEquation, and that there are no
-- type signatures without equations
toFunction :: [Def a] -> Function a
toFunction [x]    = Function (getName x) [x] Nothing
toFunction (d:ds) = case d of
  DTypeSig id t -> Function id ds (Just t)
  -- In this case there was no declared type signature
  _             -> Function (getName d) ds Nothing
\end{code}

Before we can begin typechecking we need to add some machinery for the Hindley Milner inference algorithm.
It is possible to generate and solve constraints as the program is being typechecked, but I thought
it seemed much simpler to collect constraints during typechecking and then solve them all at once in
the end.

First we need to define substitutions. A substitution is simply a map from identifiers to types, and an
entry in this map means that when we see a type variable with the identifier we should replace it
with the type in the map.
\begin{code}
type Subst = Map.Map Ident Type
\end{code}

The unit substitution is the empty substitution.
\begin{code}
unitsub :: Subst
unitsub = Map.empty
\end{code}
If we have two substitutions we need to be able to compose them. Now we run into an issue, however!
What if one substitution maps the typevariable \verb|a| to the concrete type \verb|Int|, and the second
substitution contains a mapping from some variable \verb|b| to a concrete type \verb|Maybe a|? When
we apply the substitution after we solved the constraints we would replace the type \verb|b| with 
the type \verb|Maybe a|, which is wrong. It should be \verb|Maybe Int|. The solution to this is to
as we are composing the substitutions also refine one of them. With other words, we need to
apply one substitution to another substitution. Let's make a typeclass for this.

\begin{code}
class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set Ident -- free type variables

instance Substitutable a => Substitutable [a] where
  apply s = map (apply s)
  ftv     = foldr (Set.union . ftv) Set.empty

-- Left biased substitution
compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1
\end{code}

The most important piece of this puzzle is of course subsituting types.
\begin{code}
instance Substitutable Type where
  apply s t = case t of
    TVar id     -> Map.findWithDefault (TVar id) id s
    TInt        -> TInt
    TLam t1 t2  -> TLam (apply s t1) (apply s t2)
    TPair t1 t2 -> TPair (apply s t1) (apply s t2)

  ftv t = case t of
    TVar id     -> Set.singleton id
    TInt        -> Set.empty
    TLam t1 t2  -> Set.union (ftv t1) (ftv t2)
    TPair t1 t2 -> Set.union (ftv t1) (ftv t2)
\end{code}

Great, now we have a representation of substitutions and we know how to substitute types.
After typechecking we will have a type annotated AST where some types are type variables, and if
type unifications succeeded we also have a substitution. Now we need to be able to apply
a substitution over such an AST. We enable to good ol' flexible instances and get to work.
\begin{code}
instance Substitutable (Pat Type) where
  apply s p = case p of
    PVar a id    -> PVar (apply s a) id
    PConst a l   -> PConst (apply s a) l
    PTup a p1 p2 -> PTup (apply s a) p1 p2

  -- We will never actually need this function for patterns, or anything other
  -- than types for that matter.
  ftv p = undefined
\end{code}
\begin{code}
instance Substitutable (Exp Type) where
  apply s e = case e of
    EVar a id      -> EVar (apply s a) id
    ELit a l       -> ELit (apply s a) l
    EAdd a e1 e2   -> EAdd (apply s a) (apply s e1) (apply s e2)
    ELam a p e     -> ELam (apply s a) (apply s p) (apply s e)
    EApp a e1 e2   -> EApp (apply s a) (apply s e1) (apply s e2)
    ELet a p e1 e2 -> ELet (apply s a) (apply s p) (apply s e1) (apply s e2)

  ftv e = undefined
\end{code}
\begin{code}
instance Substitutable (Def Type) where
  apply s d = case d of
    DTypeSig id t         -> DTypeSig id t
    DEquation t id args e -> DEquation (apply s t) id (map (apply s) args) (apply s e)

  ftv = undefined

instance Substitutable (Function Type) where
  apply s f = f { equations = map (apply s) (equations f)
                , typesig   = maybe Nothing (Just . apply s) (typesig f)
                }

  ftv = undefined
\end{code}

Great! Now if we have an annotated tree and a substitution we can refine the tree.
\begin{code}
refine :: [Function Type] -> Subst -> [Function Type]
refine funs subst = map (apply subst) funs
\end{code}

Now we get to the slightly messy but still quite straight forward idea of constraint solving.
As we are typechecking we are going to emit constraints that says "these two types need to be unified,
or there is a type error in the program". Unification can fail for a couple of reasons, so let's begin
with that.
\begin{code}
data UnificationError = OccursError Ident Type
                      | UnificationError Type Type

instance Show UnificationError where
  show e = case e of
    OccursError id t -> concat ["Can not substitute ", show id
                               , " for ", show t
                               , "! the type does not become more concrete."
                               ]
    UnificationError t1 t2 -> concat ["Can not unify the two types "
                                     , show t1, " and ", show t2
                                     ]
\end{code}
We raise \verb|OccursError| if we encounter an infinite type while typechecking. If we try to substitute
the type variable \verb|a| for \verb|Maybe a|, the type does not become more concrete. In fact, it becomes
bigger. If we repeatedly apply the same substitution to this type the type will only grow bigger as we
are nesting Maybe's \verb|Maybe (Maybe (Maybe ...))|. \verb|UnificationError| is thrown when a program
is ill-typed, meaning we have tried to unify e.g \verb|Int| and \verb|Int -> Int|.

Back to the constraints! Let's define a datatype for them.
\begin{code}
data Constraint = Constraint Type Type

instance Substitutable Constraint where
  apply s (Constraint t1 t2) = Constraint (apply s t1) (apply s t2)
  
  ftv = undefined
\end{code}

When we are solving these constraints we are trying to build a substitution. We can use a monad to
help us raise the errors.
\begin{code}
type Unification a = Except UnificationError a
\end{code}

Unifying two types is not as much work as it might seem. We need to think of just a couple of things.
As soon as we try to unify a type variable with \textit{anything} else, we can create a substitution
mapping this variable to whatever the other thing was. We also need to make sure that the shape of
the two types are the same. In any other case we throw a unification error.
\begin{code}
unify :: Type -> Type -> Unification Subst
unify (TPair t1 t2) (TPair t1' t2') = do
  s1 <- unify t1 t1'
  s2 <- unify t2 t2'
  return $ s2 `compose` s1

unify (TLam t1 t2) (TLam t1' t2') = do
  s1 <- unify t1 t1'
  s2 <- unify t2 t2'
  return $ s2 `compose` s1

unify (TVar id) t = bind id t
unify t (TVar id) = bind id t
unify TInt TInt   = return unitsub
unify t1 t2       = throwError $ UnificationError t1 t2

bind :: Ident -> Type -> Unification Subst
bind id t
  | t == TVar id          = return unitsub
  | id `Set.member` ftv t = throwError $ OccursError id t
  | otherwise             = return $ Map.singleton id t
\end{code}

Now that we know how to unify two types we can figure out how to unify all constraints
we will be collecting during type checking. When we have unified one constraint we need to
refine the rest of the constraints. If we have the two constraints
\begin{verbatim}
c1 = Constraint (TVar (Ident "a")) TInt
c2 = Constraint (TVar (Ident "b")) (TLam (TVar (Ident "a")) TInt)
\end{verbatim}
we can refine \verb|c2| after unifying \verb|c1|. Otherwise the substitution will, when it
substitutes the type variable \verb|b| just reintroduce the type variable \verb|a|.
\begin{code}
unifyConstraints :: [Constraint] -> Either UnificationError Subst
unifyConstraints cons = runExcept $ runUnify (cons, Map.empty)
  where
      runUnify :: ([Constraint], Subst) -> Unification Subst
      runUnify ([], s)                    = return s
      runUnify ((Constraint t1 t2:cs), s) = do
        s1 <- unify t1 t2
        runUnify ((map (apply s1) cs), (s1 `compose` s))
\end{code}

Now we have enough machinery in place to begin collecting constraints and solving them!
To typecheck a program we need to maintain an environment of identifiers and their types.
\begin{spec}
data Env = Env (Map.Map Ident Type)

instance Substitutable Env where
  apply s (Env m) = Env $ Map.map (apply s) m

  ftv (Env m) = ftv $ Map.elems m
\end{spec}

Let's consider the identity function. It's type is \verb|a -> a|. When we
typecheck \verb|id 3| the substitution will in the end contain a mapping from \verb|a| to \verb|Int|.
What if we now also want to typecheck \verb|id (\x -> x + 5)|? The type \verb|a| should now be unified
with \verb|Int -> Int| instead, but we already established that the type of \verb|a| must be \verb|Int|.
The solution to this is to use type schemas!
\begin{code}
data Schema = Forall [Ident] Type
  deriving Show

instance Substitutable Schema where
  -- Since the variable in `vars` are bound by the forall any mapping involving
  -- them in the substitution must be removed before the substitution is applied.
  apply s (Forall vars t) = Forall vars $ apply s' t
    where s' = foldr Map.delete s vars

  ftv (Forall vars t) = ftv t `Set.difference` Set.fromList vars
\end{code}

A type schema looks at a type and binds the free type variables in the type in a forall construct. If
our typing environment contains a mapping from \verb|a| to some type, then \verb|x : a -> Int| will
have the type schema \verb|forall [] . a -> Int|. If the variable \verb|a| is not known to us, however,
this means that the type is free, and the schema is instead \verb|forall [a] . a -> Int|.

Our environment will map identifiers to schemas now, rather than to types as before.
\begin{code}
data Env = Env (Map.Map Ident Schema)
  deriving Show

emptyEnv :: Env
emptyEnv = Env Map.empty

instance Substitutable Env where
  apply s (Env m) = Env $ Map.map (apply s) m

  ftv (Env m) = ftv $ Map.elems m
\end{code}

What we will do is that when we insert a mapping from an identifier to its type in the environment
we will \textit{generalize} it, meaning we will determine which variables are free and which are bound.
\begin{code}
generalize :: Type -> Env -> Schema
generalize t env = Forall vars t
  where
      vars :: [Ident]
      vars = Set.toList $ ftv t `Set.difference` ftv env
\end{code}

Conversely, when we lookup the type of a variable in the environment we will find a type schema that
we will want to \textit{instantiate}. This is done by replacing all the variables bound by the
forall with freshly generated, unique type variables. Now we need to introduce some typechecking-monad
so that we can generate fresh names.
Our typechecking monad will be a transformer with State and Reader. The Reader environment holds the
type information that maps identifiers to types. Using a Reader rather than state here makes it easier
to work with scoping. The state just maintains a counter that is used to generate fresh names. We use
an Except monad to be able to throw some errors and a writer monad to output constraints to solve.
\begin{code}
data TCError = UnboundVariable Ident

instance Show TCError where
  show e = case e of
    UnboundVariable id -> "can not resolve symbol: " ++ show id

data TCState = TCState { namegen :: Int }

type TC a = WriterT [Constraint] (
            ExceptT TCError (
            StateT TCState (
            Reader Env)))
            a
\end{code}
\begin{spec}
fresh :: TC Type
fresh = ...
\end{spec}
\ignore{
\begin{code}
letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: TC Type
fresh = do
  s <- get
  put $ s { namegen = namegen s + 1}
  return $ TVar (Ident (letters !! namegen s))
\end{code}
}
\begin{code}
-- Notice that if the Schema doesn't contain any free variables the result of this
-- function is just the type `t` in `Forall vars t`, unchanged
instantiate :: Schema -> TC Type
instantiate (Forall vars t) = do
  freshtypevars <- mapM (const fresh) vars
  let s = Map.fromList $ zip vars freshtypevars
  return $ apply s t
\end{code}

We need some convenience functions to work with the environment as we are typechecking.
\begin{code}
lookupVar :: Ident -> TC Type
lookupVar id = do
  (Env env) <- ask
  case Map.lookup id env of
    Just schema -> instantiate schema
    Nothing     -> throwError $ UnboundVariable id
\end{code}

We will also need to be able to add and remove type information from the environment.
\begin{code}
extend :: Env -> Ident -> Schema -> Env
extend (Env m) id schema = Env $ Map.insert id schema m

restrict :: Env -> Ident -> Env
restrict (Env m) id = Env $ Map.delete id m
\end{code}

When we want to extend the environment, however, this is because we've just assertained the type of a
variable, and we need to extend the envinronment with this information before we continue. In all of these
circumstances, however, the scope in which this variable should exist in the environment is limited.
Consider this example.
\begin{verbatim}
let x = 5
in x + x
\end{verbatim}
In our AST this would be represented as
\begin{verbatim}
ELet () (PVar () (Ident "x")) (ELit () (LInt 5))
        (EAdd () (EVar () (Ident "x")) (EVar () (Ident "x")))
\end{verbatim}

As soon as we have assertained what type \verb|x| has we should extend the environment with this
new information before we typecheck \verb|x + x|. This information should, however, only be available
when we are typechecking \verb|x + x| and should be forgotten after we are done. We define a
function that takes a list of new information for the environment and another typechekcking computation,
and only updates the environment in that computation. Before we add a mapping we need to remove the
old mapping if there was one, as we allow variables to overshadow each other.
\begin{code}
inEnv :: Ident -> Schema -> TC a -> TC a
inEnv id schema ma = inEnvMany [(id,schema)] ma

inEnvMany :: [(Ident, Schema)] -> TC a -> TC a
inEnvMany xs ma =
  let scope e = foldl (\e' (id,schema) -> extend (restrict e' id) id schema) e xs
  in local scope ma
\end{code}

Now we are almost ready to start typechecking! We would like a nicer convenience function for outputting
constraints rather than always writing \verb|tell [Constraint t1 t2]|.
\begin{code}
uni :: Type -> Type -> TC ()
uni t1 t2 = tell [Constraint t1 t2]
\end{code}

Let's add function stubs for the typechecking functions we will need. As we are annotating the tree as
we are typechecking it all functions need to return a tree.
\begin{spec}
checkFunction :: Function () -> TC (Function Type)
checkFunction f = undefined

checkDef :: Def () -> TC (Def Type)
checkDef d = undefined

checkPat :: Pat () -> TC (Pat Type)
checkPat p = undefined

checkExp :: Exp () -> TC (Exp Type)
checkExp e = undefined
\end{spec}

Let's begin with typechecking patterns. Patterns create new variables so what we want to do is
to assign fresh type variables to them. We don't need to unify any types at this point.
\begin{code}
checkPat :: Pat () -> TC (Pat Type)
checkPat p = case p of
  PVar () id    -> do
    t <- fresh
    return $ PVar t id
  PConst () l   ->
    return $ PConst (constType l) l
  PTup () p1 p2 -> do
    p1' <- checkPat p1
    p2' <- checkPat p2
    let tuptype = TPair (patVar p1') (patVar p2')
    return $ PTup tuptype p1' p2'

constType :: Lit -> Type
constType (LInt _) = TInt
\end{code}

We will also need to be able to fetch all new variables and their types that the pattern bound, so
we create a function that does just that.
\begin{code}
patBindings :: Pat Type -> [(Ident, Type)]
patBindings p = case p of
  PVar t id    -> [(id,t)]
  PConst _ l   -> []
  PTup _ p1 p2 -> patBindings p1 ++ patBindings p2

checkPatAndBindings :: Pat () -> TC (Pat Type, [(Ident, Type)])
checkPatAndBindings p = do
  p' <- checkPat p
  let bindings = patBindings p'
  return (p', bindings)
\end{code}

Let's continue with checking definitions. For type signatures there's no work to be done as the type
parameter is essentially a phantom type. Equations are a little more involved! First of all the
equation might have arguments (patterns). We always have an expressions. If we say that we have two
arguments with type \verb|t1| and \verb|t2|, and the expression has type \verb|t3|, then the entire
equation must have the type \verb|t1 -> t2 -> t3|. Furthermore, we must extend the environment with the
patterns and their types before we typecheck the expression/body of the equation. We don't need to
unify any types at this point either.
\begin{code}
checkDef :: Def () -> TC (Def Type)
checkDef d = case d of
  DTypeSig id t             -> return $ DTypeSig id t
  DEquation () id args body -> do
    -- annotate patterns and get their variable bindings
    args'          <- mapM checkPat args
    let argbindings = concat $ map patBindings args'

    -- fetch the environment and transform arginbinds from [(Ident, Type)]
    -- to [(Ident, Schema)] so that we can extend the environment before
    -- typechecking the body
    env            <- ask
    let argschemas  = map (\(id,t) -> (id, generalize t env)) argbindings

    -- typecheck the body in the extended environment
    body'          <- inEnvMany argschemas $ checkExp body

    -- construct the function type of this definition and return the new
    -- annotated definition
    let functype    = foldr TLam (expVar body') $ map snd argbindings
    return $ DEquation functype id args' body'
\end{code}

Now let's move on to typechecking \verb|Function ()| values. Now we will need to perform some unification.
A function consists of maybe a type signature and one or more function equations. What we need to do is
of course to typecheck all the equations. However, we also need to unify all equations to make sure that
they are all of the same type, and if there exists a type signature we should unify the equations with that
signature also.
\begin{code}
checkFunction :: Function () -> TC (Function Type)
checkFunction f = do
  -- typecheck equations and fetch their types
  eqs      <- mapM checkDef $ equations f
  let types = map (fromJust . defVar) $ filter (isJust . defVar) eqs
  
  -- unify all the types of the equations and with the typesig, if
  -- one exists
  unifyEquations types
  maybe (return ()) (uni (head types)) (typesig f)
  
  -- return annotated functions
  return $ f { equations = eqs }
  where
      unifyEquations :: [Type] -> TC ()
      unifyEquations []       = return ()
      unifyEquations [x]      = return ()
      unifyEquations (x:y:xs) = uni x y >> unifyEquations (y:xs)
\end{code}

Now we get to the part that might be a little bit meaty, typechecking expressions.
Variables are very easy, we just look up the type from the environment and annotate the variable with it.
Literals are trivial and additions are simple (typecheck the two subexpressions and unify their types with
\verb|TInt|).
\begin{spec}
checkExp :: Exp () -> TC (Exp Type)
checkExp e = case e of
  EVar () id      -> do
    t <- lookupVar id
    return $ EVar t id
  ELit () l       -> do
    return $ ELit (constType l) l
  EAdd () e1 e2   -> do
    e1' <- checkExp e1
    e2' <- checkExp e2
    let t1 = expVar e1'
    let t2 = expVar e2'
    uni t1 TInt
    uni t2 TInt
    return $ EAdd TInt e1' e2'
\end{sped}

Let's just to a trickier case, lambda abstractions. In a lambda we need to typecheck the pattern to some
type \verb|a|, and then typecheck the expression to some type \verb|b| after having extended the
environment, and then type the entire lambda abstraction as having type \verb|a -> b|.
\begin{spec}
  ELam () p e     -> do
    (p',vars) <- checkPatAndBindings p
    env       <- ask
    let vars'  = map (\(id,t) -> (id, generalize t env)) vars
    e'        <- inEnvMany vars' $ checkExp e 
    let ptype  = patVar p'
    let etype  = expVar e'
    let lamtyp = TLam ptype etype
    return $ ELam lamtyp p' e'
\end{spec}

Application is a little simpler as we do not need to extend our typing environment, but we need to
do some unification. We need to typecheck \verb|e2|, generate a fresh type variable \verb|c| and then
make sure that the type of \verb|e1| is a function with the appropriate type.
\begin{sped}
  EApp () e1 e2   -> do
    e1'  <- checkExp e1
    e2'  <- checkExp e2
    let a = expVar e1'
    let b = expVar e2'
    c    <- fresh
    uni a (TLam b c)
    return $ EApp c e1' e2'
\end{spec}

The last case, where we ran into issues with the old typechecker. We need to extend the environment
with any variables we create in the pattern, unify them with the type of expression \verb|e1| and then
compute the type of the entire let-expression by typechecking \verb|e2|.
\begin{spec}
  ELet () p e1 e2 -> do
    (p',vars) <- checkPatAndBindings p
    e1'       <- checkExp e1
    let tp     = patVar p'
    let te1    = expVar e1'
    uni tp te1

    -- Here we manually create `Forall [] t` rather than using generalize
    -- since we know that we just generated fresh type variables for these
    -- variables. If we used generalize they would be considered free and would
    -- be replaced with fresh variables every time we performed a lookup on them.
    let vars'  = map (\(id,t) -> (id, Forall [] t)) vars
    e2'       <- inEnvMany vars' $ checkExp e2
    return $ ELet (expVar e2') p' e1' e2'
\end{spec}

Now let's move on to typechecking an entire program. It is quite straight forward really, we just need
to typecheck one function, extend the environment with the name and type of that function and then
typecheck the rest of the functions.
\begin{code}
checkProgram :: [Function ()] -> TC [Function Type]
checkProgram []     = return []
checkProgram (f:fs) = do
  f'        <- checkFunction f
  let id     = name f'
  --           brave call to fromJust here...
  let ty     = fromJust $ defVar $ head $ equations f'
  env       <- ask
  let schema = generalize ty env
  fs'       <- inEnv id schema $ checkProgram fs
  return $ f' : fs'
\end{code}

Now we are ready to typechecking a program! We write a function that takes a program, typechecks it
and returns the annotated tree and a substitutation that can be applied on the tree.
\begin{code}
typecheck :: [Def ()] -> Either String ([Function Type], Subst)
typecheck defs =
  let written = runWriterT $ checkProgram funs
      excepted = runExceptT written
      stated = evalStateT excepted (TCState 0)
      readed = runReader stated (Env Map.empty)
  in case readed of
    Left err -> Left $ show err
    Right (annotated,constraints) -> case unifyConstraints constraints of
      Left err    -> Left $ show err
      Right subst -> Right (annotated, subst)
  where
      funs :: [Function ()]
      funs = mkFunctions defs
\end{code}

\ignore{
\begin{code}
test :: [Def ()]
test = [ DEquation () (Ident "baz") [] (ELet () (PVar () (Ident "x")) (ELit () (LInt 5)) ((EVar () (Ident "x"))))
       , DEquation () (Ident "main") [] (EVar () (Ident "baz"))
       ]
\end{code}
}

Now, coding up the example that we failed before
\begin{spec}
maz = let x = 5
      in x

main = baz
\end{spec}

Typechecking fails. The entire let-expression is typed correctly to \verb|TInt| and so is \verb|baz|,
but \verb|main| gets an unknown type \verb|b|. Why is this?

It is important to remember that during typechecking a \textit{lot} of things gets assigned a freshly
generated type variable which is then unified with a more concrete type. This unification is done after the
whole tree has been annotated, however, so while \verb|baz| is initially given the type \verb|a|, when we
apply the substitution it is resolved with \verb|Int|. If another function such as \verb|main| needs
to lookup the type of \verb|baz| during typechecking, all it will see is \verb|a|. Now, this is quite
fine as we have emitted constraints that unify \verb|a| with \verb|Int|, so what is exactly causing
the issue?

After the let-expression has been annotated and, in this case, assigned a fresh type variable, we want
to grab that type and associate it with \verb|baz| in our environment, so that subsequent usages can
lookup the type of \verb|baz|. Here is the thing: when we are doing this in \verb|checkFunction| we
have forgotten the type information in the environment that the type of the let-expression does not
actually contain free type variables, and we will go on to generalize it. This will give us a new,
fresh type variable when we lookup the type, and this type will not be unified with the inferred type
from before, resulting in a type variable surviving even after we apply the substitution.

The root of the issue is that we are calling \verb|generalize| with an environment that is not
representative of what the environment looked like when we determined the type we are generalizing.
At the same time the call to generalize is crucial for us to be able to handle polymorphic functions.
E.g the type of \verb|map| would be inferred to some nice polymorphic type, and if we don't generalize
this type when we are putting it in the environment we could only ever apply it at a single type.

One option is to not annotate things with types, but rather to annotate things with schemas, I suppose.
This way the schema could be generated at the point where the environment is representative. It seems awfully
messy to pass schemas around, though.

Another alternative is to solve constraints as we are typechecking, rather than collecting all the
constraints and solving them at the end. The downside of this is however that it would mean mixing
unification code and typechecking code in the same places. At this point I don't see any other
solution though, sad to say.

\begin{code}

checkExp :: Exp () -> TC (Exp Type)
checkExp e = case e of
  EVar () id      -> do
    t <- lookupVar id
    return $ EVar t id
  ELit () l       -> do
    return $ ELit (constType l) l
  EAdd () e1 e2   -> do
    e1' <- checkExp e1
    e2' <- checkExp e2
    let t1 = expVar e1'
    let t2 = expVar e2'
    uni t1 TInt
    uni t2 TInt
    return $ EAdd TInt e1' e2'
  ELam () p e     -> do
    (p',vars) <- checkPatAndBindings p
    env       <- ask
    let vars'  = map (\(id,t) -> (id, generalize t env)) vars
    e'        <- inEnvMany vars' $ checkExp e 
    let ptype  = patVar p'
    let etype  = expVar e'
    let lamtyp = TLam ptype etype
    return $ ELam lamtyp p' e'
  EApp () e1 e2   -> do
    e1'  <- checkExp e1
    e2'  <- checkExp e2
    let a = expVar e1'
    let b = expVar e2'
    c    <- fresh
    uni a (TLam b c)
    return $ EApp c e1' e2'
  ELet () p e1 e2 -> do
    (p',vars) <- checkPatAndBindings p
    e1'       <- checkExp e1
    let tp     = patVar p'
    let te1    = expVar e1'
    uni tp te1
    let vars'  = map (\(id,t) -> (id, Forall [] t)) vars
    e2'       <- inEnvMany vars' $ checkExp e2
    return $ ELet (expVar e2') p' e1' e2'
\end{code}





















































\begin{spec}
data Lit = LInt Int
         | LFloat Double
  deriving (Eq, Show)

data Type = TVar Ident
          | TInt
          | TFloat
          | TPair Type Type
          | TLam Type Type
  deriving (Eq, Show)

data BinOp a = Add a
             | Sub a
             | Mul a
             | Div a
  deriving (Eq, Show, Ord)

binOpVar :: BinOp a -> a
binOpVar op = case op of
  Add a -> a
  Sub a -> a
  Mul a -> a
  Div a -> a

setBinOpVar :: BinOp a -> b -> BinOp b
setBinOpVar op b = case op of
  Add _ -> Add b
  Sub _ -> Sub b
  Mul _ -> Mul b
  Div _ -> Div b

instance Substitutable (BinOp Type) where
  apply s o = case o of
    Add a -> Add $ apply s a
    Sub a -> Sub $ apply s a
    Mul a -> Mul $ apply s a
    Div a -> Div $ apply s a

  ftv _ = undefined

data Exp a = EVar a Ident
           | ELit a Lit
           | EBin a (Exp a) (Exp a) (BinOp a)
           | ELam a (Pat a) (Exp a)
           | EApp a (Exp a) (Exp a)
           | ELet a (Pat a) (Exp a) (Exp a)
  deriving (Eq, Show)

expVar :: Exp a -> a
expVar e = case e of
  EVar a _     -> a
  ELit a _     -> a
  EBin a _ _ _ -> a
  ELam a _ _   -> a
  ELet a _ _ _ -> a

instance Substitutable Type where
  apply s t = case t of
    TVar id     -> Map.findWithDefault (TVar id) id s
    TInt        -> TInt
    TFloat      -> TFloat
    TLam t1 t2  -> TLam (apply s t1) (apply s t2)
    TPair t1 t2 -> TPair (apply s t1) (apply s t2)

  ftv t = case t of
    TVar id     -> Set.singleton id
    TInt        -> Set.empty
    TFloat      -> Set.empty
    TLam t1 t2  -> Set.union (ftv t1) (ftv t2)
    TPair t1 t2 -> Set.union (ftv t1) (ftv t2)

instance Substitutable (Exp Type) where
  apply s e = case e of
    EVar a id      -> EVar (apply s a) id
    ELit a l       -> ELit (apply s a) l
    EBin a e1 e2 o -> EBin (apply s a) (apply s e1) (apply s e2) (apply s o)
    ELam a p e     -> ELam (apply s a) (apply s p) (apply s e)
    EApp a e1 e2   -> EApp (apply s a) (apply s e1) (apply s e2)
    ELet a p e1 e2 -> ELet (apply s a) (apply s p) (apply s e1) (apply s e2)

  ftv e = undefined

unify :: Type -> Type -> Unification Subst
unify (TPair t1 t2) (TPair t1' t2') = do
  s1 <- unify t1 t1'
  s2 <- unify t2 t2'
  return $ s2 `compose` s1

unify (TLam t1 t2) (TLam t1' t2') = do
  s1 <- unify t1 t1'
  s2 <- unify t2 t2'
  return $ s2 `compose` s1

unify (TVar id) t   = bind id t
unify t (TVar id)   = bind id t
unify TInt TInt     = return unitsub
unify TFloat TFloat = return unitsub
unify t1 t2         = throwError $ UnificationError t1 t2

bind :: Ident -> Type -> Unification Subst
bind id t
  | t == TVar id          = return unitsub
  | id `Set.member` ftv t = throwError $ OccursError id t
  | otherwise             = return $ Map.singleton id t

ops :: Map.Map (BinOp ()) Schema
ops = Map.fromList $
    [ (Add (), Forall [id] (TLam tv (TLam tv tv)))
    , (Sub (), Forall [id] (TLam tv (TLam tv tv)))
    , (Mul (), Forall [id] (TLam tv (TLam tv tv)))
    , (Div (), Forall [id] (TLam tv (TLam tv tv)))
    ]
  where
      id :: Ident
      id = Ident "a"

      tv :: Type
      tv = TVar id

data Constraint = Constraint Type Type
                | OneOf [(Type, Type)]

instance Substitutable Constraint where
  apply s (Constraint t1 t2) = Constraint (apply s t1) (apply s t2)
  apply s (OneOf xs)         = OneOf $ map (\(t1,t2) -> (apply s t1, apply s t2)) xs
  
  ftv = undefined

unifyConstraints :: [Constraint] -> Either UnificationError Subst
unifyConstraints cons = runExcept $ runUnify (cons, Map.empty)
  where
      runUnify :: ([Constraint], Subst) -> Unification Subst
      runUnify ([], s)                    = return s
      runUnify (OneOf ts:cs, s)           = do
        s1 <- unifyOneOf ts
        runUnify ((map (apply s1) cs), (s1 `compose` s))
      runUnify ((Constraint t1 t2:cs), s) = do
        s1 <- unify t1 t2
        runUnify ((map (apply s1) cs), (s1 `compose` s))

unifyOneOf :: [(Type, Type)] -> Unification Subst
unifyOneOf []           = return unitsub
unifyOneOf [(t1,t2)]    = unify t1 t2
unifyOneOf ((t1,t2):ts) = catchError (unify t1 t2) $ \e ->
  unifyOneOf ts

lookupOp :: BinOp () -> TC (BinOp Type)
lookupOp op = case Map.lookup op ops of
  Just schema -> do t <- instantiate schema
                    unifyOp t op
                    return $ setBinOpVar op t
  Nothing     -> throwError undefined

uniMany :: [(Type, Type)] -> TC ()
uniMany ts = tell [OneOf ts]

unifyOp :: Type -> BinOp a -> TC ()
unifyOp t op = case op of
  Add _ -> uniMany [(t, floatfun), (t, intfun)]
  Sub _ -> uniMany [(t, floatfun), (t, intfun)]
  Mul _ -> uniMany [(t, floatfun), (t, intfun)]
  Div _ -> uniMany [(t, floatfun), (t, intfun)]
  where
      intfun :: Type
      intfun = TLam TInt (TLam TInt TInt)

      floatfun :: Type
      floatfun = TLam TFloat (TLam TFloat TFloat)

checkExp :: Exp () -> TC (Exp Type)
checkExp e = case e of
  EVar () id      -> do
    t <- lookupVar id
    return $ EVar t id
  ELit () l       -> do
    return $ ELit (constType l) l
  EBin () e1 e2 o -> do -- TODO change here
    e1'       <- checkExp e1
    e2'       <- checkExp e2
    let t1     = expVar e1'
    let t2     = expVar e2'
    t3        <- fresh
    let infopt = TLam t1 (TLam t2 t3)
    o'        <- lookupOp o
    uni infopt (binOpVar o')
    return $ EBin t3 e1' e2' o'
  ELam () p e     -> do
    (p',vars) <- checkPatAndBindings p
    env       <- ask
    let vars'  = map (\(id,t) -> (id, generalize t env)) vars
    e'        <- inEnvMany vars' $ checkExp e 
    let ptype  = patVar p'
    let etype  = expVar e'
    let lamtyp = TLam ptype etype
    return $ ELam lamtyp p' e'
  EApp () e1 e2   -> do
    e1'  <- checkExp e1
    e2'  <- checkExp e2
    let a = expVar e1'
    let b = expVar e2'
    c    <- fresh
    uni a (TLam b c)
    return $ EApp c e1' e2'
  ELet () p e1 e2 -> do
    (p',vars) <- checkPatAndBindings p
    e1'       <- checkExp e1
    let tp     = patVar p'
    let te1    = expVar e1'
    uni tp te1
    env       <- ask
    let vars'  = map (\(id,t) -> (id, generalize t env)) vars
    e2'       <- inEnvMany vars' $ checkExp e2
    return $ ELet (expVar e2') p' e1' e2'
\end{spec}
\end{document}