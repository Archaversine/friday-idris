module Friday.Parsing.Expression

import Data.Fin
import Data.String
import Data.String.Parser
import Data.String.Parser.Expression

import Friday.Optimize.Expression
import public Friday.Types.Expression

parseIntExpr : Parser Expr 
parseIntExpr = IntExpr . cast <$> (integer <* spaces)

parseDoubleExpr : Parser Expr
parseDoubleExpr = do 
    sign   <- optional (char '-')
    first  <- concatMap show <$> some digit 
    second <- concatMap show <$> (char '.' *> some digit <* spaces)

    pure $ case sign of 
        Nothing => DoubleExpr $ cast (first ++ "." ++ second)
        Just  _ => DoubleExpr $ cast ("-" ++ first ++ "." ++ second)

parseStringExpr : Parser Expr
parseStringExpr = StringExpr <$> (char '"' *> takeUntil "\"" <* spaces)

parseBoolExpr : Parser Expr
parseBoolExpr = do 
    b <- (string "true" $> BoolExpr True) <|> (string "false" $> BoolExpr False)
    spaces *> pure b

export
parseIdentifier : Parser String 
parseIdentifier = do 
    first  <- singleton <$> letter 
    second <- pack <$> (many alphaNum <* spaces)

    pure (first ++ second)

parseIdentifierExpr : Parser Expr
parseIdentifierExpr = IdentifierExpr <$> parseIdentifier

openParen : Parser ()
openParen = char '(' *> spaces

closeParen : Parser () 
closeParen = char ')' *> spaces

opTable : OperatorTable Expr
opTable = [ [ Prefix (char '-' *> spaces $> UnaryExpr Negate) 
            , Prefix (char '!' *> spaces $> UnaryExpr Not)
            ]
            ,
            [ Infix (char '*' *> spaces $> BinaryExpr Mul) AssocLeft
            , Infix (char '/' *> spaces $> BinaryExpr Div) AssocLeft
            ]
            ,
            [ Infix (char '+' *> spaces $> BinaryExpr Add) AssocLeft
            , Infix (char '-' *> spaces $> BinaryExpr Sub) AssocLeft
            ]
            ,
            [ Infix (char '<'    *> spaces $> BinaryExpr Lt)  AssocLeft
            , Infix (char '>'    *> spaces $> BinaryExpr Gt)  AssocLeft 
            , Infix (string ">=" *> spaces $> BinaryExpr Ge)  AssocLeft
            , Infix (string "<=" *> spaces $> BinaryExpr Le)  AssocLeft
            , Infix (string "==" *> spaces $> BinaryExpr Eq)  AssocLeft
            , Infix (string "!=" *> spaces $> BinaryExpr Neq) AssocLeft
            ]
            , 
            [ Infix (string "&&" *> spaces $> BinaryExpr And) AssocLeft
            ]
            ,
            [ Infix (string "||" *> spaces $> BinaryExpr Or) AssocLeft
            ]
          ]

mutual
    parseTerenaryExpr : Parser Expr 
    parseTerenaryExpr = do 
        cond <- string "if"   *> spaces *> parseUnreducedExpr 
        exp1 <- string "then" *> spaces *> parseUnreducedExpr
        exp2 <- string "else" *> spaces *> parseUnreducedExpr

        pure (TerenaryExpr cond exp1 exp2)

    parseFuncCallExpr : Parser Expr 
    parseFuncCallExpr = do 
        name <- parseIdentifier
        args <- openParen *> sepBy parseUnreducedExpr (char ',' *> spaces) <* closeParen

        pure (FuncCallExpr name args)

    parseFactor : Parser Expr
    parseFactor = parseIntExpr  
              <|> parseDoubleExpr 
              <|> parseStringExpr 
              <|> parseBoolExpr 
              <|> parseTerenaryExpr 
              <|> parseFuncCallExpr 
              <|> parseIdentifierExpr 
              <|> (openParen *> parseUnreducedExpr <* closeParen)

    export
    parseUnreducedExpr : Parser Expr
    parseUnreducedExpr = buildExpressionParser Expr opTable parseFactor

export 
parseExpr : Parser Expr 
parseExpr = reduceExpr <$> parseUnreducedExpr