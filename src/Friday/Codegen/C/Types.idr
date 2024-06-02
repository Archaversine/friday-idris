module Friday.Codegen.C.Types

import public Friday.Codegen.Types

public export
CGen : Type -> Type
CGen = CodeGen "c" FileError

export
HasCodeGen CGen where 
    generate = defaultGenerate
    pregen   = generateLine (generate "#include <stdio.h>\n")

