module Compile (interface, implementation) where

import Values
import ObjCSyntax
import ObjC
import qualified Data.Char as C
import Prelude hiding (compare)
import Data.List (isInfixOf)
import Data.Char (toLower)

interface :: Value -> Interface
interface v@(Value name fields) = Interface name (map property fields) [initializerMethod fields]

property :: Field -> Property
property (Field t@(Scalar _) name) = Property [Nonatomic] t name
property (Field t name) | t == Pointer (Scalar "NSString") = Property [Nonatomic,Copy] t name
                        | isDelegate name = Property [Nonatomic,Weak] t name
                        | otherwise = Property [Nonatomic,Strong] t name

isDelegate name = "delegate" `isInfixOf` map toLower name


implementation :: Value -> Implementation
implementation v@(Value name _) = Implementation name (methods v)

methods (Value name fields) =
  [ initializerMethod fields
  , isEqual name fields
  ] ++ (hash fields)

initializerMethod :: [Field] -> Method
initializerMethod fields =
  cm InstanceType (initializerMethodName fields) [
      if_ self (map assignment fields)
    , return_ self
  ]

isEqual name fields = im bool (NameWithArguments [Argument "isEqual" Id otherName]) $ [
     if_ (not_ $ msg1 other "isKindOfClass" $ msg (v name) "class") [return_ no]
   , var myClass "person" .=. CastExpr myClass (v otherName)
 ] ++ map compare fields ++ [finalCompare fields]
 where myClass = Pointer (Scalar name)

hash [] = []
hash fields = return $ im (Scalar "NSUInteger") (SimpleName "hash") $
   [ var (Scalar "NSUInteger") "hash" .=. msg super "hash"
   ] ++ map hashField fields ++ [ return_ (v "hash") ]
 where hashField (Field (Scalar _) x) = (v "hash") .=. ((v "hash") .*. (Other "31u") .+. msg (AtExpr $ self <.> v x) "hash")
       hashField (Field _ x) = (v "hash") .=. ((v "hash") .*. (Other "31u") .+. msg (self <.> v x) "hash")

compare (Field (Scalar _) fieldName) = var bool (boolNameForField fieldName) .=. (ivarName fieldName .==. ivar other (ivarName fieldName))
compare (Field _ fieldName) = var bool (boolNameForField fieldName) .=. msg1 (ivarName fieldName) "isEqual" (ivar other $ ivarName fieldName)

finalCompare [] = return_ yes
finalCompare ls = return_ $ foldl1 and_ $ map (v . boolNameForField . name) ls

otherName = "other"
other = v otherName

boolNameForField n = n ++ "IsEqual"

ivarName :: String -> Expression
ivarName = v . (++) "_"

initializerMethodName [] = SimpleName "init"
initializerMethodName (x:xs) = NameWithArguments (firstParameter x : map parameterForField xs)

firstParameter :: Field -> Argument
firstParameter (Field t name) = Argument ("initWith" ++ capitalize name) t name

parameterForField :: Field -> Argument
parameterForField (Field t name) = Argument name t name

assignment :: Field -> Statement
assignment (Field _ n) = self <.> v n .=. v n

capitalize :: String -> String
capitalize (c:cs) = (C.toUpper c) : cs
capitalize [] = []
