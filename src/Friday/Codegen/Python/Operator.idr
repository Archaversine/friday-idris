module Friday.Codegen.Python.Operator

import Friday.Types.Operations

import Friday.Codegen.Python.Types

export
genUnaryOp : FUnary -> PyGen () 
genUnaryOp Negate = generate "-"
genUnaryOp Not    = generate "not "

export
genBinaryOp : FBinary -> PyGen ()
genBinaryOp Add = generate " + "
genBinaryOp Sub = generate " - "
genBinaryOp Mul = generate " * "
genBinaryOp Div = generate " / "
genBinaryOp And = generate " and "
genBinaryOp Or  = generate " or "
genBinaryOp Lt  = generate " < "
genBinaryOp Gt  = generate " > "
genBinaryOp Le  = generate " <= "
genBinaryOp Ge  = generate " >= "
genBinaryOp Eq  = generate " == "
genBinaryOp Neq = generate " != "
