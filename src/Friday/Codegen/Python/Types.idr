module Friday.Codegen.Python.Types

import public Friday.Codegen.Types

public export
PyGen : Type -> Type
PyGen = CodeGen "python" FileError

export
Backend "python" FileError where 
    generate = defaultGenerate
    postgen  = generateLine (generate "main()")

