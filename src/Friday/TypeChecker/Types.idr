module Friday.TypeChecker.Types

import public Control.Monad.State

import public Data.SortedMap

import Friday.Types.Types
import Friday.Types.Expression

public export 
record TypeContext where 
    constructor MkTypeContext 
    types : SortedMap String FridayType 
    preds : SortedMap String Expr

export
data TypeChecker a = MkChecker (State TypeContext (Either String a))

builtinTypes : SortedMap String FridayType
builtinTypes = fromList [("print", IOFnc FVoid [FString])]

export 
runChecker : TypeChecker a -> Either String a 
runChecker (MkChecker x) = evalState (MkTypeContext builtinTypes empty) x

export 
runCheckerWith : TypeContext -> TypeChecker a -> Either String a 
runCheckerWith ctx (MkChecker x) = evalState ctx x

export 
typeError : String -> TypeChecker a
typeError err = MkChecker $ ST $ \s => pure (s, Left err)

fromChecker : TypeChecker a -> State TypeContext (Either String a)
fromChecker (MkChecker x) = x

export
Functor TypeChecker where 
    map f (MkChecker s) = MkChecker (map (map f) s)

export
Applicative TypeChecker where 
    pure = MkChecker . pure . pure
    MkChecker f <*> MkChecker x = MkChecker $ (<*>) <$> f <*> x

export
Monad TypeChecker where 
    MkChecker x >>= f = MkChecker $ ST $ \s => do 
        let (s', e) = runState s x
        pure $ case e of 
            Left err => (s', Left err)
            Right a  => runState s' (fromChecker (f a))

export 
MonadState TypeContext TypeChecker where 
    get    = MkChecker $ ST $ \s => pure (s, Right s)
    put s' = MkChecker $ ST $ \_ => pure (s', Right ())

export
addType : String -> FridayType -> TypeChecker () 
addType name ty = gets (lookup name . types) >>= \case 
    Just ty2 => typeError $ name ++ " has conflicting types " ++ show ty2 ++ " and " ++ show ty
    Nothing  => modify (\s => { types := insert name ty s.types} s)

export
addTypeOverride : String -> FridayType -> TypeChecker () 
addTypeOverride name ty = modify (\s => { types := insert name ty s.types } s)

export 
addPredOverride : String -> Expr -> TypeChecker () 
addPredOverride name p = modify (\s => { preds := insert name p s.preds } s)
