{-# LANGUAGE OverloadedStrings #-}

module Parser (parse) where

import Values
import ObjCSyntax

import qualified Data.Attoparsec.Text as A
import Data.Attoparsec.Text ((.*>),  many1)
import Control.Applicative
import qualified Data.Text as T

parse :: T.Text -> Either String Value
parse = A.parseOnly parser

parser :: A.Parser Value
parser = Value <$> parseStart <*> parseFields

parseStart :: A.Parser String
parseStart = ("value" .*> spaces) *> identifierName <* maybeSpaces

spaces :: A.Parser String
spaces = A.many1 A.space

maybeSpaces :: A.Parser String
maybeSpaces = many A.space

token :: T.Text -> A.Parser T.Text
token t = A.string t <* maybeSpaces

parseFields :: A.Parser [Field]
parseFields = token "{" *> (commaList parseField) <* maybeSpaces <* token "}"

commaList :: A.Parser a -> A.Parser [a]
commaList item = item `A.sepBy` token ","

parseField :: A.Parser Field
parseField = Field <$> typeName <*> identifierName

typeName :: A.Parser Type
typeName = combine <$> (idType <|> pointer <|> scalar) <*> protocols
 where combine x [] = x
       combine x xs = QualifiedWithProtocols x xs

protocols :: A.Parser [String]
protocols = token "<" *> commaList identifierName <* token ">"
         <|> pure []

idType :: A.Parser Type
idType = Id <$ A.string "id" <* spaces

pointer :: A.Parser Type
pointer = f <$> scalar <*> many1 star
 where f t [] = t
       f t (_:xs) = f (Pointer t) xs

scalar :: A.Parser Type
scalar = Scalar <$> identifierName <* maybeSpaces

star :: A.Parser T.Text
star = token "*"

identifierName :: A.Parser String
identifierName = A.many1 A.letter
