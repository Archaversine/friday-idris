module Friday.TypeChecker.Expression

import Data.List

import Friday.TypeChecker.Types

import Friday.Optimize.Expression
import Friday.Types.Expression
import Friday.Types.Types

export
validTypes : Either FBinary FUnary -> List FridayType
validTypes (Left Add)     = [FInt, FDouble]
validTypes (Left Sub)     = [FInt, FDouble]
validTypes (Left Mul)     = [FInt, FDouble]
validTypes (Left Div)     = [FInt, FDouble]
validTypes (Left And)     = [FBool]
validTypes (Left Or)      = [FBool]
validTypes (Left Lt)      = [FInt, FDouble, FBool]
validTypes (Left Gt)      = [FInt, FDouble, FBool]
validTypes (Left Le)      = [FInt, FDouble, FBool]
validTypes (Left Ge)      = [FInt, FDouble, FBool]
validTypes (Left Eq)      = [FInt, FDouble, FBool]
validTypes (Left Neq)     = [FInt, FDouble, FBool]
validTypes (Right Negate) = [FInt, FDouble]
validTypes (Right Not)    = [FBool]

mutual
    export
    checkFuncParams : String -> List FridayType -> List Expr -> TypeChecker ()
    checkFuncParams str tys exprs = do 
        args <- traverse typeOf exprs 
        case and $ zipWith (\a, b => delay (a == b)) tys args of 
            True  => pure ()
            False => typeError $ "Arguments for function call " ++ str ++ " have incorrect types."

    export
    typeOf : Expr -> TypeChecker FridayType 
    typeOf (IntExpr i)           = pure FInt
    typeOf (DoubleExpr dbl)      = pure FDouble
    typeOf (StringExpr str)      = pure FString
    typeOf (BoolExpr x)          = pure FBool
    typeOf (IdentifierExpr str)  = gets (lookup str . types) >>= \case 
        Nothing => typeError $ "Untyped " ++ str ++ "."
        Just ty => pure ty
    typeOf (UnaryExpr x y)       = typeOf y >>= \case 
        ty => case ty `elem` validTypes (Right x) of 
            True  => pure ty
            False => typeError $ show y ++ " has an invalid type for unary operator " ++ show x ++ "."
    typeOf (BinaryExpr x y z)    = do 
        ty1 <- typeOf y 
        ty2 <- typeOf z 

        case (ty1 == ty2) && (ty1 `elem` validTypes (Left x)) of 
            True  => pure $ case x of 
                Lt  => FBool
                Gt  => FBool
                Le  => FBool
                Ge  => FBool
                Eq  => FBool
                Neq => FBool
                _   => ty1
            False => typeError $ show y ++ " and " ++ show z ++ " do not satisfy the type for operator " ++ show x
    typeOf (TerenaryExpr x y z)  = typeOf x >>= \case 
        FBool => do 
            ty1 <- typeOf y 
            ty2 <- typeOf z 

            case ty1 == ty2 of 
                True  => pure ty1 
                False => typeError $ show y ++ " and " ++ show z ++ " have different types in terenary."
        _     => typeError $ show x ++ " should be of type Bool in terenary expression."
    typeOf (FuncCallExpr str xs) = gets (lookup str . types) >>= \case 
        Nothing => typeError $ "Untyped function call to " ++ str ++ "."
        Just (FFunc ret ts) => gets (lookup str . preds) >>= \case 
            Nothing => checkFuncParams str (map snd ts) xs *> pure ret
            Just  p => case reduceExpr (substituteMany (zip (map fst ts) xs) p) of 
                BoolExpr True => pure ret 
                _             => typeError $ "Predicate " ++ show p ++ " is not satisfied."
        Just (IOFnc ret ts) => checkFuncParams str ts xs *> pure ret
        _ => typeError $ "Call to non function " ++ str ++ "."


export 
checkNotIOExpr : Expr -> TypeChecker () 
checkNotIOExpr (IntExpr i)           = pure ()
checkNotIOExpr (DoubleExpr dbl)      = pure ()
checkNotIOExpr (StringExpr str)      = pure ()
checkNotIOExpr (BoolExpr x)          = pure ()
checkNotIOExpr (IdentifierExpr str)  = pure ()
checkNotIOExpr (UnaryExpr x y)       = pure ()
checkNotIOExpr (BinaryExpr x y z)    = pure ()
checkNotIOExpr (TerenaryExpr x y z)  = pure ()
checkNotIOExpr (FuncCallExpr str xs) = gets (lookup str . types) >>= \case 
    Just (IOFnc ret ts) => typeError $ "Function call " ++ str ++ " is impure."
    _ => pure ()

