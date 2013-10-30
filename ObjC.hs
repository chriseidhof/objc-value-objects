module ObjC where

import ObjCSyntax

im,cm :: ReturnType -> MethodName -> Body -> Method
im = Method Instance
cm = Method Class

(<.>), (.*.), (.+.) :: Expression -> Expression -> Expression

(<.>) = PropertyExpr
l .*. r = OperatorExpr l "*" r
l .+. r = OperatorExpr l "+" r

v :: String -> Expression
v = VariableExpr

if_ :: Expression -> Body -> Statement
if_ = IfStatement

return_ = ReturnStatement
no = Other "NO"
yes = Other "NO"
not_ = NotExpr
msg1 x sel y = MessageExpr x (SelectorWithArguments [sel]) [y]
msg x sel = MessageExpr x (SelectorNoArgument sel) []

l .=. r = AssignmentStatement l Equals r
l .==. r = OperatorExpr l "==" r

bool :: Type
bool = Scalar "BOOL"

self = VariableExpr "self"
super = VariableExpr "super"

var t = VarExpr t . v
ivar = IVarExpr
and_ :: Expression -> Expression -> Expression
and_ l r = OperatorExpr l "&&" r
