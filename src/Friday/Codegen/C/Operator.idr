module Friday.Codegen.C.Operator

import Friday.Types.Operations

import Friday.Codegen.C.Types

export
genUnaryOp : FUnary -> CGen () 
genUnaryOp Negate = generate "-"
genUnaryOp Not    = generate "!"

export
genBinaryOp : FBinary -> CGen ()
genBinaryOp Add = generate " + "
genBinaryOp Sub = generate " - "
genBinaryOp Mul = generate " * "
genBinaryOp Div = generate " / "
genBinaryOp And = generate " && "
genBinaryOp Or  = generate " || "
genBinaryOp Lt  = generate " < "
genBinaryOp Gt  = generate " > "
genBinaryOp Le  = generate " <= "
genBinaryOp Ge  = generate " >= "
genBinaryOp Eq  = generate " == "
genBinaryOp Neq = generate " != "
