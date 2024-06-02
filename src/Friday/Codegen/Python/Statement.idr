module Friday.Codegen.Python.Statement

import Friday.Types.Statement

import Friday.Codegen.Python.Types
import Friday.Codegen.Python.Operator
import Friday.Codegen.Python.Expression

generateFuncArgs : List (String, FridayType) -> PyGen ()
generateFuncArgs []                 = pure ()
generateFuncArgs (x :: xs@(_ :: _)) = generate (fst x ++ ", ") *> generateFuncArgs xs
generateFuncArgs (x :: xs)          = generate (fst x)

mutual
    generateIfStmt : List Stmt -> Expr -> PyGen ()
    generateIfStmt xs x = do 
        generateLine $ generate "if " *> genExpr x *> generate ":"
        withIndent (traverse_ genStmt xs)

    generateForStmt : List Stmt -> Stmt -> Expr -> Stmt -> PyGen ()
    generateForStmt xs x y z = do 
        genStmt x
        generateLine $ generate "while " *> genExpr y *> generate ":"
        withIndent $ do 
            traverse_ genStmt xs 
            genStmt z

    generateFuncDeclStmt : List Stmt -> FridayType -> List (String, FridayType) -> String -> PyGen ()
    generateFuncDeclStmt xs x ys str = do 
        generateLine $ generate ("def " ++ str) *> inParens (generateFuncArgs ys) *> generate ":"
        withIndent (traverse_ genStmt xs)

    generateIOFuncDeclStmt : List Stmt -> FridayType -> List (String, FridayType) -> String -> PyGen ()
    generateIOFuncDeclStmt xs x ys str = do 
        generateLine $ generate ("def " ++ str) *> inParens (generateFuncArgs ys) *> generate ":"
        withIndent (traverse_ genStmt xs)

    export
    genStmt : Stmt -> PyGen ()
    genStmt (LetStmt str x y)            = generateLine (generate (str ++ " = ") *> genExpr y)
    genStmt (VarStmt str x y)            = generateLine (generate (str ++ " = ") *> genExpr y)
    genStmt (MutateStmt str x)           = generateLine (generate (str ++ " = ") *> genExpr x)
    genStmt (IfStmt x xs)                = generateIfStmt xs x
    genStmt (ForStmt x y z xs)           = generateForStmt xs z y x
    genStmt (FuncCallStmt str xs)        = generateLine (generate str *> inParens (genFuncArgs xs))
    genStmt (FuncDeclStmt str xs x m ys) = generateFuncDeclStmt ys x xs str
    genStmt (FuncDeclStmtIO str xs x ys) = generateIOFuncDeclStmt ys x xs str
    genStmt (ReturnStmt x)               = generateLine (generate "return " *> genExpr x)
    genStmt BreakStmt                    = generateLine (generate "break")
    genStmt ContinueStmt                 = generateLine (generate "continue")
