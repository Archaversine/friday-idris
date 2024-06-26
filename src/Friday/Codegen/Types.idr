module Friday.Codegen.Types

import public Control.Monad.Reader
import public Control.Monad.Error.Either

import public Data.String

import public System.File.ReadWrite

public export
record CodeGenState where 
    constructor MkCodeGenState 
    output : String 
    indent : Nat

public export
record CodeGen (0 s : String) (e : Type) (a : Type) where 
    constructor MkCodeGen
    runCodeGen' : EitherT e (ReaderT CodeGenState IO) a

newCodeGen : String -> CodeGenState 
newCodeGen path = MkCodeGenState path 0

export 
runCodeGen : HasIO io => String -> CodeGen s e a -> io (Either e a)
runCodeGen path g = liftIO $ runReaderT (newCodeGen path) (runEitherT (runCodeGen' g))

export 
Functor (CodeGen s e) where 
    map f (MkCodeGen e) = MkCodeGen (map f e)

export 
Applicative (CodeGen s e) where 
    pure = MkCodeGen . pure 

    MkCodeGen f <*> MkCodeGen x = MkCodeGen (f <*> x)

export 
Monad (CodeGen s e) where 
    MkCodeGen x >>= f = MkCodeGen $ do
        result <- map f x
        runCodeGen' result

export 
MonadReader CodeGenState (CodeGen s e) where
    local f c = MkCodeGen $ MkEitherT $ do 
        s <- ask
        local f (runEitherT (runCodeGen' c))
    ask = MkCodeGen ask 

export
HasIO (CodeGen s e) where 
    liftIO = MkCodeGen . liftIO

public export 
interface Backend (0 s : String) (0 e : Type) | s where 
    generate : String -> CodeGen s e ()

    withIndent : CodeGen s e a -> CodeGen s e a
    withIndent = local (\s => { indent := S s.indent} s)

    generateIndent : CodeGen s e () 
    generateIndent = do 
        spaces <- asks indent 
        generate (replicate spaces ' ')

    generateLine : CodeGen s e a -> CodeGen s e () 
    generateLine line = generateIndent *> line *> generate "\n"

    pregen : CodeGen s e ()
    pregen = pure ()

    postgen : CodeGen s e ()
    postgen = pure ()

export 
defaultGenerate : String -> CodeGen s FileError ()
defaultGenerate text = do 
    path <- asks output 
    appendFile path text >>= \case 
        Left err => MkCodeGen (left err) 
        Right () => pure ()

export 
clearOutput : CodeGen s FileError ()
clearOutput = do 
    path <- asks output 
    writeFile path "" >>= \case 
        Left err => MkCodeGen (left err)
        Right () => pure ()