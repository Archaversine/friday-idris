module Friday.Parsing.FTypes 

import Data.String.Parser

import Friday.Types.Types

parseFTypeLiteral : Parser FridayType
parseFTypeLiteral = string "Int"    $> FInt 
                <|> string "Double" $> FDouble 
                <|> string "String" $> FString
                <|> string "Bool"   $> FBool
                <|> string "Void"   $> FVoid

export 
parseFType : Parser FridayType 
parseFType = parseFTypeLiteral <* spaces
