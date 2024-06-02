module Friday.TypeChecker.Statement

import Data.List

import Friday.Optimize.Expression

import Friday.Types.Statement
import Friday.TypeChecker.Types
import Friday.TypeChecker.Expression

retStmtExpr : Stmt -> Maybe Expr 
retStmtExpr (ReturnStmt e) = Just e 
retStmtExpr _ = Nothing

checkReturns : FridayType -> List Stmt -> TypeChecker ()
checkReturns ty [] = pure () 
checkReturns ty (ReturnStmt e :: xs) = do 
    t <- typeOf e 
    case t == ty of 
        True  => pure ()
        False => typeError $ "Return " ++ show e ++ " does not return type " ++ show ty ++ "."
checkReturns ty (_ :: xs) = checkReturns ty xs

readContextSingle : Stmt -> TypeChecker ()
readContextSingle (LetStmt str x y)            = addType str x
readContextSingle (VarStmt str x y)            = addType str x
readContextSingle (FuncDeclStmt str xs x Nothing ys ) = addType str (FFunc x xs)
readContextSingle (FuncDeclStmt str xs x (Just m) ys) = do 
    addType str (FFunc x xs)
    addPredOverride str m
readContextSingle (FuncDeclStmtIO str xs x ys) = addType str (IOFnc x (map snd xs))
readContextSingle _                            = pure ()

export
readContext : List Stmt -> TypeChecker ()
readContext xs = traverse_ readContextSingle xs

checkNoFuncDecs : List Stmt -> TypeChecker () 
checkNoFuncDecs [] = pure ()
checkNoFuncDecs ((FuncDeclStmt str ys x m zs) :: xs) = typeError $ "Function " ++ str ++ " cannot be defined here."
checkNoFuncDecs ((FuncDeclStmtIO str ys x zs) :: xs) = typeError $ "Function " ++ str ++ " cannot be defined here."
checkNoFuncDecs (_ :: xs) = checkNoFuncDecs xs

checkPredicate : Expr -> TypeChecker ()
checkPredicate (IntExpr i)           = typeError $ "Predicate expected bool expr, got " ++ show i   ++ " instead."
checkPredicate (DoubleExpr dbl)      = typeError $ "Predicate expected bool expr, got " ++ show dbl ++ " instead."
checkPredicate (StringExpr str)      = typeError $ "Predicate expected bool expr, got " ++ show str ++ " instead."
checkPredicate (BoolExpr x)          = pure ()
checkPredicate (IdentifierExpr str)  = gets (lookup str . types) >>= \case 
    Just FBool => pure ()
    _          => typeError $ str ++ " cannot be used as a predicate expr."
checkPredicate (UnaryExpr Negate y)  = typeError $ "Numeric expr " ++ show y ++ " cannot be used as predicate expr."
checkPredicate (UnaryExpr Not y)     = checkNotIOExpr y
checkPredicate (BinaryExpr And y z)  = checkNotIOExpr y *> checkNotIOExpr z
checkPredicate (BinaryExpr Or y z)   = checkNotIOExpr y *> checkNotIOExpr z
checkPredicate (BinaryExpr Lt y z)   = checkNotIOExpr y *> checkNotIOExpr z
checkPredicate (BinaryExpr Gt y z)   = checkNotIOExpr y *> checkNotIOExpr z
checkPredicate (BinaryExpr Le y z)   = checkNotIOExpr y *> checkNotIOExpr z
checkPredicate (BinaryExpr Ge y z)   = checkNotIOExpr y *> checkNotIOExpr z
checkPredicate (BinaryExpr Eq y z)   = checkNotIOExpr y *> checkNotIOExpr z
checkPredicate (BinaryExpr Neq y z)  = checkNotIOExpr y *> checkNotIOExpr z
checkPredicate (BinaryExpr op y z)   = typeError $ "Binary operator " ++ show op ++ " does not return a Bool type."
checkPredicate (TerenaryExpr x y z)  = typeOf y >>= \case 
    FBool => checkNotIOExpr y *> checkNotIOExpr z
    _     => typeError "Terenary expr does not return bool in predicate."
checkPredicate (FuncCallExpr str xs) = pure ()

checkNotImpure : Stmt -> TypeChecker () 
checkNotImpure (LetStmt str x y) = checkNotIOExpr y
checkNotImpure (VarStmt str x y) = typeError $ str ++ " is impure."
checkNotImpure (MutateStmt str x) = typeError $ str ++ " is impurely mutated."
checkNotImpure (IfStmt x xs) = traverse_ checkNotImpure xs
checkNotImpure (ForStmt x y z xs) = typeError $ "For loops are not supported inside pure functions yet."
checkNotImpure (FuncCallStmt str xs) = gets (lookup str . types) >>= \case 
    Just (IOFnc t ts) => typeError $ "IO Function call to " ++ str ++ " inside pure function."
    _                 => pure ()
checkNotImpure (FuncDeclStmt str xs x m ys) = pure () -- Ignore; function's won't be inside functions
checkNotImpure (FuncDeclStmtIO str xs x ys) = pure () -- Ignore; function's won't be inside functions
checkNotImpure (ReturnStmt x)               = checkNotIOExpr x
checkNotImpure BreakStmt                    = pure ()
checkNotImpure ContinueStmt                 = pure ()

withFuncContext : List (String, FridayType) -> TypeChecker a -> TypeChecker a
withFuncContext params tc = do 
    s <- get
    case runCheckerWith s (traverse_ (uncurry addTypeOverride) params *> tc) of 
        Left err => typeError err
        Right re => pure re

export
checkType : Stmt -> TypeChecker ()
checkType (LetStmt str x y) = do 
    ty <- typeOf y

    case x == ty of 
        True  => pure ()
        False => typeError $ show y ++ " does not match the type annotation " ++ show x ++ " for " ++ str ++ "."
checkType (VarStmt str x y) = do 
    ty <- typeOf y 

    case x == ty of 
        True  => pure () 
        False => typeError $ show y ++ " does not match the type annotation " ++ show x ++ " for " ++ str ++ "."
checkType (MutateStmt str x) = gets (lookup str . types) >>= \case 
    Nothing => typeError $ "Untyped variable " ++ str ++ "."
    Just ty => do 
        ty2 <- typeOf x 
        case ty == ty2 of 
            True  => pure ()
            False => typeError $ "Type mismatch for variable " ++ str ++ " and expr " ++ show x ++ "."
checkType (IfStmt x xs) = typeOf x >>= \case
    FBool => traverse_ checkType xs
    _     => typeError $ "If statement condition " ++ show x ++ " is not a boolean."
checkType (ForStmt x y z xs) = typeOf y >>= \case 
    FBool => pure () 
    _     => typeError $ "For loop condition " ++ show y ++ " is not a boolean."
checkType (FuncCallStmt str xs) = gets (lookup str . types) >>= \case 
    Nothing => typeError $ "Untyped " ++ str ++ " function call."
    Just (FFunc t ts) => gets (lookup str . preds) >>= \case 
        Nothing => pure ()
        Just  p => case reduceExpr (substituteMany (zip (map fst ts) xs) p) of 
            BoolExpr True => pure ()
            _             => typeError $ "Predicate " ++ show p ++ " is not satisfied in function call."
    Just (IOFnc t ts) => checkFuncParams str ts xs
    _ => typeError $ "Attempted to call non function type " ++ str ++ "."
checkType (FuncDeclStmt str xs x Nothing ys) = do 
    checkNoFuncDecs ys
    traverse_ checkNotImpure ys
    withFuncContext xs (checkReturns x ys *> traverse_ checkType ys)
checkType (FuncDeclStmt str xs x (Just m) ys) = do
    checkNoFuncDecs ys
    checkPredicate m
    traverse_ checkNotImpure ys
    withFuncContext xs (checkReturns x ys *> traverse_ checkType ys)
checkType (FuncDeclStmtIO str xs x ys) = do 
    checkNoFuncDecs ys
    withFuncContext xs (checkReturns x ys *> traverse_ checkType ys)
checkType (ReturnStmt x) = pure ()
checkType BreakStmt      = pure ()
checkType ContinueStmt   = pure ()

export
checkTypes : List Stmt -> TypeChecker () 
checkTypes xs = traverse_ checkType xs