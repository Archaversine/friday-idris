module Friday.Codegen.Python

import Friday.Types.Statement

import Friday.Codegen.Python.Statement
import Friday.Codegen.Python.Types

import System.File.ReadWrite 

export 
writePython : HasIO io => String -> List Stmt -> io (Either FileError ()) 
writePython path xs = runCodeGen path (clearOutput *> pregen *> traverse_ genStmt xs *> postgen)
