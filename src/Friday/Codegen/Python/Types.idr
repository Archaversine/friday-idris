module Friday.Codegen.Python.Types

import public Friday.Codegen.Types

public export
PyGen : Type -> Type
PyGen = CodeGen "python" FileError

export
HasCodeGen PyGen where 
    generate = defaultGenerate
    postgen  = generateLine (generate "main()")

