module Example where

import Parser
import qualified Compile as C
import ObjCSyntax
import ObjC
import PPObjCSyntax
import qualified Data.Text as T

driver = case parse (T.pack exampleStr) of
           Left err -> error err
           Right ast -> (implementation (C.implementation ast), interface (C.interface ast))

exampleStr = "value Test { NSString* string, id<MyDelegate> delegate, NSUInteger int, id object }"

example = Implementation "Test" [
          Method Instance (Scalar "void") (SimpleName "init") [
            IfStatement (Other "[hello world]") [
              AssignmentStatement (PropertyExpr self $ VariableExpr "x") Equals (VariableExpr "y")
            ]
          ],
          Method Class (Scalar "instancetype") (NameWithArguments [Argument "testWithString" (Pointer $ Scalar "String") "string"]) [
            ReturnStatement (Other $ "[[self alloc] init]")
          ]
        ]
