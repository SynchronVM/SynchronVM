{- | This module exposes a datatype that is used to signal typechecking errors -}
module CamIoT.Typecheck.TCError where

import           CamIoT.Internal.Syntax
import           CamIoT.Pretty.Syntax

{- | This datatype declaration declares all the different kinds of type errors that
can be thrown during type checking. -}
data TCError
    {- | @UnboundVariable x@ reports that during type checking we have seen the
    variable @x@, but that @x@ has not been declared anywhere. E.g
    
    @
    main = (\x -> y) 5 -- error, y is unbound
    @
    -}
    = UnboundVariable Ident
    {- | @UnknownConstructor c@ reports that we have seen a data constructor which
    was not declared in any of the datatype declarations. E.g
    
    @
    data Maybe a where
      Just : a -> Maybe a
    
    f (Just 5) = 5
    f Nothing  = 10 -- here we raise the error, since Nothing doesn't exist
    @
    
    -}
    | UnknownConstructor UIdent
    {- | @UnknownBinop op@ says that the binary oprator @op@ is unknown. E.g
    
    @
    a {}+ c -- what does this even mean?
    @

    -}
    | UnknownBinop (Binop ())
    {- | This error is thrown during unification to report that an occurs check failed.
    The same kind of error where Haskell says "can not construct infinite type..." -}
    | OccursError Ident Type
    {- | @UnificationError t1 t2@ reports that the two types @t1@ and @t2@ could not be
    unified, while the typechecker requires them to be.
    
    @
    if 5 then x else y -- unification error! Can not unify Int (from the 5) with Bool
    @
    
    -}
    | UnificationError Type Type
    {- | @UndeclaredTycon tc@ says that the type constructor @tc@ was not declared in any
    datatype declarations.
    
    @
    data Maybe a where
      Just : a -> Maybe a
      Nothing : Maybe a
    
    mToi : Either a b -> Maybe a -- error here! Either undeclared
    mToi (Left a) = Just a
    mToi _        = Nothing
    @
    
    -}
    | UndeclaredTycon UIdent
    {- | @DuplicateTycon tc@ says that the type constructor @tc@ has been declared twice.

    @
    data Maybe a where
      Just : a -> Maybe a
    
    data Maybe a where -- error! Maybe already declared
      Nothing : Maybe a
    @

    -}
    | DuplicateTycon UIdent
    {- | @DuplicateDataConstructor dc@ is the same as @DuplicateTycon@, but for data
    constructors.
    
    @
    @
    
    -}
    | DuplicateDataConstructor UIdent
    {- | @PartiallyAppliedTycon tc i1 i2@ says that the type constructor @tc@ was applied
    to @i1@ type arguments, but it expected @i2@. @i2@ is strictly greater than @i1@.
    
    @
    data Either a b where
      Left  : a -> Either a b
      Right : b -> Either a b
    
    -- error here! Either is not a type, it's a function from a type to a type
    fromLeft : Either a -> a
    fromLeft (Left a) = a
    @
    
    -}
    | PartiallyAppliedTycon UIdent Int Int
    {- | TODO -}
    | UnboundADTVariable UIdent [Ident] UIdent Type Ident
    {- | This error is related to GADTs. If a datatype declaration declares something
    that is not an algebraic data type, but more closely resembles a GADT, this error
    is thrown. Example:
    
    @
    data Exp a where
      EInt : Int -> Exp Int -- Not a normal ADT, but a GADT!
    @
    
    -}
    | NonADTConstruction UIdent Type Type
    {- | If the inferred type of a function is less general than that of the declared
    type signature, this error is reported. The types can however be unified.
    
    @
    -- error! The inferred type of the result is definitely Int, but the declared type is
    -- a
    f : Int -> a
    f i = i + 5
    @
    
    -}
    | TypeSignatureTooGeneral Ident Type Type
    {- | This error is thrown when the inferred type and the declared type is
    different.
    
    @
    -- error, the declared type is completely different form the inferred type
    f : Int -> Int
    f True = Just 5
    @
    
    -}
    | TypesigError Ident Type Type

-- | Show instance for type errors
instance Show TCError where
  show e = case e of
    UnboundVariable    id  -> "can not resolve symbol: " ++ show id
    UnknownConstructor uid -> "can not resolve constructor: " ++ show uid
    UnknownBinop       op  -> "can not resolve binop: " ++ printTree op
    OccursError id t       -> concat
      [ "Can not substitute "
      , show id
      , " for "
      , show t
      , "! the type does not become more concrete."
      ]
    UnificationError t1 t2 -> concat
      ["Can not unify the two types ", printTree t1, " and ", printTree t2]
    UndeclaredTycon uid ->
      concat ["Undeclared type constructor: ", printTree uid]
    DuplicateTycon uid ->
      concat ["Type constructor ", printTree uid, " already declared"]
    DuplicateDataConstructor uid ->
      concat ["Data constructor ", printTree uid, " already declared"]
    PartiallyAppliedTycon uid expectedarity actualarity -> concat
      [ "Type constructor "
      , printTree uid
      , " is partially applied; expected arity is "
      , show expectedarity
      , " but actual arity is "
      , show actualarity
      ]
    UnboundADTVariable tycon vars datacon t unexpected -> concat
      [ "The data constructor "
      , printTree datacon
      , " declared with "
      , printTree tycon
      , " "
      , printTree vars
      , ", is declared to have type "
      , printTree t
      , ", but the data declaration only binds variables ["
      , printTree vars
      , "]. The "
      , "variable "
      , printTree unexpected
      , " is unbound and unexpected"
      ]
    NonADTConstruction datacon expected actual -> concat
      [ "The data constructor "
      , printTree datacon
      , " constructs a value of type "
      , printTree actual
      , ", but the expected type is "
      , printTree expected
      ]
    TypeSignatureTooGeneral fun declared inferred -> concat
      [ "The type signature of "
      , printTree fun
      , " is too general:\n"
      , "  declared: "
      , printTree declared
      , "\n"
      , "  inferred: "
      , printTree inferred
      ]
    TypesigError fun declared inferred -> concat
      [ "Type error in function "
      , printTree fun
      , ":\n"
      , "  declared type: "
      , printTree declared
      , "\n"
      , "  inferred: "
      , printTree inferred
      ]
