module Friday.Codegen.C

import Friday.Types.Statement

import Friday.Codegen.C.Statement
import Friday.Codegen.C.Types

import System.File.ReadWrite 

export 
writeC : HasIO io => String -> List Stmt -> io (Either FileError ()) 
writeC path xs = runCodeGen path (clearOutput *> pregen *> traverse_ genStmt xs *> postgen)
