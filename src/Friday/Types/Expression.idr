module Friday.Types.Expression

import public Friday.Types.Types
import public Friday.Types.Operations

public export
data Expr : Type where 
    IntExpr        : Int     -> Expr 
    DoubleExpr     : Double  -> Expr 
    StringExpr     : String  -> Expr 
    BoolExpr       : Bool    -> Expr 
    IdentifierExpr : String  -> Expr
    UnaryExpr      : FUnary  -> Expr -> Expr
    BinaryExpr     : FBinary -> Expr -> Expr -> Expr
    TerenaryExpr   : Expr    -> Expr -> Expr -> Expr
    FuncCallExpr   : String  -> List Expr -> Expr

export 
Show Expr where 
    show (IntExpr i)           = "(IntExpr "        ++ show i   ++ ")"
    show (DoubleExpr dbl)      = "(DoubleExpr "     ++ show dbl ++ ")"
    show (StringExpr str)      = "(StringExpr "     ++ show str ++ ")"
    show (BoolExpr x)          = "(BoolExpr "       ++ show x   ++ ")"
    show (IdentifierExpr str)  = "(IdentifierExpr " ++ show str ++ ")"
    show (UnaryExpr x y)       = "(UnaryExpr "      ++ show x   ++ " " ++ assert_total (show y)  ++ ")"
    show (BinaryExpr x y z)    = "(BinaryExpr "     ++ show x   ++ " " ++ assert_total (show y)  ++ " " ++ assert_total (show z) ++ ")"
    show (TerenaryExpr x y z)  = "(TerenaryExpr "   ++ show x   ++ " " ++ assert_total (show y)  ++ " " ++ assert_total (show z) ++ ")"
    show (FuncCallExpr str xs) = "(FuncCallExpr "   ++ show str ++ " " ++ assert_total (show xs) ++ ")"

export 
substitute : String -> Expr -> Expr -> Expr 
substitute name expr e@(IdentifierExpr str) = case name == str of 
    True  => expr 
    False => e
substitute name expr (UnaryExpr    x y  ) = UnaryExpr    x (substitute name expr y)
substitute name expr (BinaryExpr   x y z) = BinaryExpr   x (substitute name expr y) (substitute name expr z)
substitute name expr (TerenaryExpr x y z) = TerenaryExpr x (substitute name expr y) (substitute name expr z)
substitute name expr e = e

export
substituteMany : List (String, Expr) -> Expr -> Expr 
substituteMany [] e = e
substituteMany ((name, expr) :: xs) e = substituteMany xs (substitute name expr e)
