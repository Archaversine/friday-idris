module Friday.Optimize.Expression

import Friday.Types.Expression

export 
reduceExpr : Expr -> Expr 
reduceExpr e@(UnaryExpr Negate y) = case reduceExpr y of
    (IntExpr i)      => IntExpr (-i)
    (DoubleExpr dbl) => DoubleExpr (-dbl)
    _                => e
reduceExpr e@(UnaryExpr Not y) = case reduceExpr y of 
    (BoolExpr x) => BoolExpr (not x)
    _            => e
reduceExpr e@(BinaryExpr Add y z) = case (reduceExpr y, reduceExpr z) of 
    (IntExpr i, IntExpr i2)           => IntExpr (i + i2)
    (DoubleExpr dbl, DoubleExpr dbl2) => DoubleExpr (dbl + dbl2)
    _                        => e
reduceExpr e@(BinaryExpr Sub y z) = case (reduceExpr y, reduceExpr z) of
    (IntExpr i, IntExpr i2)           => IntExpr (i - i2)
    (DoubleExpr dbl, DoubleExpr dbl2) => DoubleExpr (dbl - dbl2)
    _                                 => e
reduceExpr e@(BinaryExpr Mul y z) = case (reduceExpr y, reduceExpr z) of 
    (IntExpr i, IntExpr i2)           => IntExpr (i * i2)
    (DoubleExpr dbl, DoubleExpr dbl2) => DoubleExpr (dbl * dbl2)
    _                                 => e
reduceExpr e@(BinaryExpr Div y z) = case (reduceExpr y, reduceExpr z) of 
    (IntExpr i, IntExpr i2)           => IntExpr (i `div` i2)
    (DoubleExpr dbl, DoubleExpr dbl2) => DoubleExpr (dbl / dbl2)
    _                                 => e
reduceExpr e@(BinaryExpr And y z) = case (reduceExpr y, reduceExpr z) of 
    (BoolExpr b, BoolExpr b2) => BoolExpr (b && b2)
    _                         => e
reduceExpr e@(BinaryExpr Or y z)  = case (reduceExpr y, reduceExpr z) of 
    (BoolExpr b, BoolExpr b2) => BoolExpr (b || b2)
    _                         => e
reduceExpr e@(BinaryExpr Lt y z)  = case (reduceExpr y, reduceExpr z) of 
    (IntExpr i, IntExpr i2)           => BoolExpr (i < i2)
    (DoubleExpr dbl, DoubleExpr dbl2) => BoolExpr (dbl < dbl2)
    _                                 => e
reduceExpr e@(BinaryExpr Gt y z)  = case (reduceExpr y, reduceExpr z) of 
    (IntExpr i, IntExpr i2)           => BoolExpr (i > i2)
    (DoubleExpr dbl, DoubleExpr dbl2) => BoolExpr (dbl > dbl2)
    _                                 => e
reduceExpr e@(BinaryExpr Le y z)  = case (reduceExpr y, reduceExpr z) of 
    (IntExpr i, IntExpr i2)           => BoolExpr (i <= i2)
    (DoubleExpr dbl, DoubleExpr dbl2) => BoolExpr (dbl <= dbl2)
    _                                 => e
reduceExpr e@(BinaryExpr Ge y z)  = case (reduceExpr y, reduceExpr z) of 
    (IntExpr i, IntExpr i2)           => BoolExpr (i >= i2)
    (DoubleExpr dbl, DoubleExpr dbl2) => BoolExpr (dbl >= dbl2)
    _                                 => e
reduceExpr e@(BinaryExpr Eq y z)  = case (reduceExpr y, reduceExpr z) of 
    (IntExpr i, IntExpr i2)           => BoolExpr (i == i2)
    (DoubleExpr dbl, DoubleExpr dbl2) => BoolExpr (dbl == dbl2)
    _                                 => e
reduceExpr e@(BinaryExpr Neq y z) = case (reduceExpr y, reduceExpr z) of 
    (IntExpr i, IntExpr i2)           => BoolExpr (i /= i2)
    (DoubleExpr dbl, DoubleExpr dbl2) => BoolExpr (dbl /= dbl2)
    _                                 => e
reduceExpr e@(TerenaryExpr x y z) = case reduceExpr x of 
    BoolExpr True  => reduceExpr y
    BoolExpr False => reduceExpr z
    _              => e
reduceExpr e = e
