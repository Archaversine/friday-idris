module Friday.Types.Statement

import public Friday.Types.Expression

public export
data Stmt : Type where
    LetStmt        : String -> FridayType -> Expr -> Stmt
    VarStmt        : String -> FridayType -> Expr -> Stmt
    MutateStmt     : String -> Expr       -> Stmt
    IfStmt         : Expr   -> List Stmt  -> Stmt
    ForStmt        : Stmt   -> Expr       -> Stmt -> List Stmt -> Stmt
    FuncCallStmt   : String -> List Expr  -> Stmt
    FuncDeclStmt   : String -> List (String, FridayType) -> FridayType -> Maybe Expr -> List Stmt -> Stmt
    FuncDeclStmtIO : String -> List (String, FridayType) -> FridayType -> List Stmt  -> Stmt
    ReturnStmt     : Expr   -> Stmt
    BreakStmt      : Stmt
    ContinueStmt   : Stmt

export
Show Stmt where 
    show (LetStmt str x y)                   = "(LetStmt "        ++ show str ++ " " ++ assert_total (show x)  ++ " " ++ assert_total (show y) ++ ")"
    show (VarStmt str x y)                   = "(VarStmt "        ++ show str ++ " " ++ assert_total (show x)  ++ " " ++ assert_total (show y) ++ ")"
    show (MutateStmt str x)                  = "(MutateStmt "     ++ show str ++ " " ++ assert_total (show x)  ++ ")"
    show (IfStmt x xs)                       = "(IfStmt "         ++ show x   ++ " " ++ assert_total (show xs) ++ ")"
    show (ForStmt x y z xs)                  = "(ForStmt "        ++ assert_total (show x) ++ assert_total (show y) ++ assert_total (show z) ++ assert_total (show xs) ++ ")"
    show (FuncCallStmt str xs)               = "(FuncCallStmt "   ++ show str ++ " " ++ assert_total (show xs) ++ ")"
    show (FuncDeclStmt str xs x Nothing ys)  = "(FuncDeclStmt "   ++ show str ++ " " ++ assert_total (show xs) ++ " " ++ assert_total (show x) ++ " " ++ assert_total (show ys) ++ ")"
    show (FuncDeclStmt str xs x (Just m) ys) = "(FuncDeclStmt "   ++ show str ++ " " ++ assert_total (show xs) ++ " " ++ assert_total (show x) ++ " [" ++ show m ++ "] " ++ assert_total (show ys) ++ ")"
    show (FuncDeclStmtIO str xs x ys)        = "(FuncDeclStmtIO " ++ show str ++ " " ++ assert_total (show xs) ++ " " ++ assert_total (show x) ++ " " ++ assert_total (show ys) ++ ")"
    show (ReturnStmt x)                      = "(ReturnStmt "     ++ show x ++ ")"
    show BreakStmt                           = "BreakStmt"
    show ContinueStmt                        = "ContinueStmt"