module Friday.Parsing.Statement

import Data.String.Parser

import Friday.Parsing.FTypes
import Friday.Parsing.Expression
import public Friday.Types.Statement

semicolon : Parser () 
semicolon = char ';' *> spaces

parseReturnStmt : Parser Stmt 
parseReturnStmt = ReturnStmt <$> (string "return" *> spaces *> parseExpr <* semicolon)

parseBreakStmt : Parser Stmt 
parseBreakStmt = string "break" *> spaces $> BreakStmt <* semicolon

parseContinueStmt : Parser Stmt 
parseContinueStmt = string "continue" *> spaces $> ContinueStmt <* semicolon

parseLetStmt : Parser Stmt 
parseLetStmt = do 
    name <- string "let" *> spaces *> parseIdentifier
    type <- char ':'     *> spaces *> parseFType
    expr <- char '='     *> spaces *> parseExpr <* semicolon

    pure (LetStmt name type expr)

parseVarStmt : Parser Stmt 
parseVarStmt = do
    name <- string "var" *> spaces *> parseIdentifier
    type <- char ':'     *> spaces *> parseFType
    expr <- char '='     *> spaces *> parseExpr <* semicolon

    pure (VarStmt name type expr)

parseMutateInline : Parser Stmt 
parseMutateInline = do 
    name <- parseIdentifier 
    expr <- char '=' *> spaces *> parseExpr

    pure (MutateStmt name expr)

parseMutateStmt : Parser Stmt 
parseMutateStmt = do 
    name <- parseIdentifier 
    expr <- char '=' *> spaces *> parseExpr <* semicolon

    pure (MutateStmt name expr)

parseFuncArg : Parser (String, FridayType)
parseFuncArg = do 
    name <- parseIdentifier 
    type <- char ':' *> spaces *> parseFType 

    pure (name, type)

mutual
    parseIfStmt : Parser Stmt 
    parseIfStmt = do 
        cond <- string "if" *> spaces *> char '(' *> spaces *> parseExpr <* char ')' <* spaces 
        cod1 <- parseBlock

        pure (IfStmt cond cod1)

    parseForStmt : Parser Stmt 
    parseForStmt = do 
        first <- string "for" *> spaces *> char '(' *> spaces *> parseVarStmt
        cond  <- parseExpr <* char ';' <* spaces
        post  <- parseMutateInline <* char ')' <* spaces
        block <- parseBlock

        pure (ForStmt first cond post block)

    parseFuncCallStmt : Parser Stmt 
    parseFuncCallStmt = do 
        name <- parseIdentifier 
        args <- char '(' *> spaces *> sepBy parseExpr (char ',' *> spaces) <* char ')' <* spaces <* semicolon

        pure (FuncCallStmt name args)

    parsePureFuncDeclStmt : Parser Stmt 
    parsePureFuncDeclStmt = do 
        name <- string "func" *> spaces *> parseIdentifier
        args <- char '(' *> spaces *> sepBy parseFuncArg (char ',' *> spaces) <* char ')' <* spaces
        pred <- optional (char '[' *> spaces *> parseExpr <* char ']' <* spaces)
        ret  <- string "->" *> spaces *> parseFType
        body <- parseBlock

        pure (FuncDeclStmt name args ret pred body)

    parseIOFuncDeclStmt : Parser Stmt 
    parseIOFuncDeclStmt = do 
        name <- string "io" *> spaces *> string "func" *> spaces *> parseIdentifier
        args <- char '(' *> spaces *> sepBy parseFuncArg (char ',' *> spaces) <* char ')' <* spaces
        ret  <- string "->" *> spaces *> parseFType
        body <- parseBlock

        pure (FuncDeclStmtIO name args ret body)

    export
    parseStmt : Parser Stmt 
    parseStmt = parseReturnStmt 
            <|> parseBreakStmt 
            <|> parseContinueStmt
            <|> parseLetStmt
            <|> parseVarStmt
            <|> parseIfStmt
            <|> parseForStmt
            <|> parsePureFuncDeclStmt
            <|> parseIOFuncDeclStmt
            <|> parseFuncCallStmt
            <|> parseMutateStmt
                     

    parseBlock : Parser (List Stmt)
    parseBlock = char '{' *> spaces *> many parseStmt <* spaces <* char '}' <* spaces

export 
parseProg : Parser (List Stmt)
parseProg = spaces *> many parseStmt <* eos