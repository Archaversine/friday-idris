module Friday.Types.Operations

public export
data FUnary = Negate | Not

public export
data FBinary
    = Add 
    | Sub 
    | Mul 
    | Div 
    | And 
    | Or 
    | Lt 
    | Gt 
    | Le 
    | Ge 
    | Eq 
    | Neq

export 
Show FUnary where 
    show Negate = "NEGATE"
    show Not    = "!"

export 
Show FBinary where 
    show Add    = "+"
    show Sub    = "-"
    show Mul    = "*"
    show Div    = "/"
    show And    = "&&"
    show Or     = "||"
    show Lt     = "<"
    show Gt     = ">"
    show Le     = "<="
    show Ge     = ">="
    show Eq     = "=="
    show Neq    = "!="