module ObjCSyntax where

data Interface = Interface Name [Property] [Method]

data Property = Property [PropertyModifier] Type Name

data PropertyModifier = Nonatomic | Strong | Weak | Copy

data Implementation = Implementation Name [Method]

data Method = Method ClassOrInstance ReturnType MethodName Body

data ClassOrInstance = Class | Instance

data MethodName = SimpleName String
                | NameWithArguments [Argument]

data Argument = Argument String Type String

data Type = Scalar String
          | Id
          | Pointer Type
          | QualifiedWithProtocols Type [String]
          | Void
          | InstanceType
 deriving (Eq, Show)

type Name = String
type Body = [Statement]

data Statement = IfStatement Expression Body
               | AssignmentStatement Expression AssignmentType Expression
               | ReturnStatement Expression
               | EmptyStatement

data Expression = Other String
                | VariableExpr String
                | PropertyExpr Expression Expression
                | NotExpr Expression
                | MessageExpr Expression Selector [Expression]
                | CastExpr Type Expression
                | VarExpr Type Expression
                | IVarExpr Expression Expression
                | OperatorExpr Expression Operator Expression
                | AtExpr Expression

type Operator = String


data Selector = SelectorNoArgument String
              | SelectorWithArguments [String]

data AssignmentType = Equals

type ReturnType = Type

-- Convenience constructors

