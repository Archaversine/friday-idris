module Friday.Codegen.C.Statement

import Friday.Types.Statement

import Friday.Codegen.C.Types
import Friday.Codegen.C.Operator
import Friday.Codegen.C.Expression

toCType : FridayType -> String
toCType FString = "const char*"
toCType FInt = "int"
toCType FDouble = "double"
toCType FBool = "int"
toCType FVoid = "void"
toCType (FFunc x xs) = "IMPOSSIBLE"
toCType (IOFnc x xs) = "IMPOSSIBLE"

generateFuncArgs : List (String, FridayType) -> CGen ()
generateFuncArgs []                 = pure ()
generateFuncArgs (x :: xs@(_ :: _)) = generate (toCType (snd x) ++ " " ++ fst x ++ ", ") *> generateFuncArgs xs
generateFuncArgs (x :: xs)          = generate (toCType (snd x) ++ " " ++ fst x)

semicolon : CGen ()
semicolon = generate ";"

mutual
    generateIfStmt : List Stmt -> Expr -> CGen ()
    generateIfStmt xs x = do 
        generateLine $ generate "if " *> inParens (genExpr x) *> generate "{"
        withIndent (traverse_ genStmt xs)
        generateLine $ generate "}"

    generateForStmt : List Stmt -> Stmt -> Expr -> Stmt -> CGen ()
    generateForStmt xs x y z = do 
        genStmt x
        generateLine $ generate "while " *> inParens (genExpr y) *> generate "{"
        withIndent $ do 
            traverse_ genStmt xs 
            genStmt z
        generateLine $ generate "}"

    generateFuncDeclStmt : List Stmt -> FridayType -> List (String, FridayType) -> String -> CGen ()
    generateFuncDeclStmt xs x ys str = do 
        generateLine $ generate (toCType x ++ " " ++ str) *> inParens (generateFuncArgs ys) *> generate "{"
        withIndent (traverse_ genStmt xs)
        generateLine $ generate "}"

    generateIOFuncDeclStmt : List Stmt -> FridayType -> List (String, FridayType) -> String -> CGen ()
    generateIOFuncDeclStmt xs x ys str = do 
        generateLine $ generate (toCType x ++ " " ++ str) *> inParens (generateFuncArgs ys) *> generate "{"
        withIndent (traverse_ genStmt xs)
        generateLine $ generate "}"

    export
    genStmt : Stmt -> CGen ()
    genStmt (LetStmt str x y)            = generateLine (generate (toCType x ++ " " ++ str ++ " = ") *> genExpr y *> semicolon)
    genStmt (VarStmt str x y)            = generateLine (generate (toCType x ++ " " ++ str ++ " = ") *> genExpr y *> semicolon)
    genStmt (MutateStmt str x)           = generateLine (generate (str ++ " = ") *> genExpr x *> semicolon)
    genStmt (IfStmt x xs)                = generateIfStmt xs x
    genStmt (ForStmt x y z xs)           = generateForStmt xs z y x
    genStmt (FuncCallStmt str xs)        = generateLine (generate str *> inParens (genFuncArgs xs) *> semicolon)
    genStmt (FuncDeclStmt str xs x m ys) = generateFuncDeclStmt ys x xs str
    genStmt (FuncDeclStmtIO str xs x ys) = generateIOFuncDeclStmt ys x xs str
    genStmt (ReturnStmt x)               = generateLine (generate "return " *> genExpr x *> semicolon)
    genStmt BreakStmt                    = generateLine (generate "break" *> semicolon)
    genStmt ContinueStmt                 = generateLine (generate "continue" *> semicolon)
