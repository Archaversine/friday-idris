module Friday.Codegen.C.Expression

import Friday.Types.Expression

import Friday.Codegen.C.Types
import Friday.Codegen.C.Operator

export
inParens : CGen a -> CGen a 
inParens g = generate "(" *> g <* generate ")"

mutual
    export
    genFuncArgs : List Expr -> CGen () 
    genFuncArgs []               = pure ()
    genFuncArgs (x :: ys@(_::_)) = genExpr x *> generate ", " *> genFuncArgs ys
    genFuncArgs (x :: Nil)       = genExpr x

    export 
    genExpr : Expr -> CGen () 
    genExpr (IntExpr i)           = generate (show i)
    genExpr (DoubleExpr dbl)      = generate (show dbl)
    genExpr (StringExpr str)      = generate (show str)
    genExpr (BoolExpr x)          = generate (show x)
    genExpr (IdentifierExpr str)  = generate str
    genExpr (UnaryExpr x y)       = inParens (genUnaryOp x *> genExpr y)
    genExpr (BinaryExpr x y z)    = inParens (genExpr y *> genBinaryOp x *> genExpr z)
    genExpr (TerenaryExpr x y z)  = inParens (genExpr y *> generate " ? " *> genExpr x *> generate " : " *> genExpr z)
    genExpr (FuncCallExpr str xs) = generate (str ++ "(") *> inParens (genFuncArgs xs) *> generate ")"

