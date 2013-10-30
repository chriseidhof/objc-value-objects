{-# LANGUAGE OverloadedStrings #-}

module PPObjCSyntax where

import ObjCSyntax
import Text.PrettyPrint
import Data.List (intersperse)

numSpaces :: Int
numSpaces = 4

interface :: Interface -> Doc
interface v@(Interface name properties methods) =
  vcat [ "@interface " <> text name
       , ""
       , vcat $ map property properties
       , ""
       , vcat $ map (s . methodDecl) methods
       , ""
       , "@end"
       ]

property (Property modifiers typ name) = "@property" <+> parens (commaList $ map modifier modifiers) <+> ppType typ <+> text name

modifier Nonatomic = "nonatomic"
modifier Strong = "strong"
modifier Weak = "weak"
modifier Copy = "copy"

implementation :: Implementation -> Doc
implementation v@(Implementation name methods) =
  vcat ["@implementation " <> text name
       ,""
       ,vcat $ map methodImpl methods
       ,""
       ,"@end"
       ]

methodDecl :: Method -> Doc
methodDecl (Method specifier returnType name _ ) = ppSpecifier specifier <+> parens (ppType returnType) <> methodName name

methodImpl :: Method -> Doc
methodImpl m@(Method _ _ _ body) = vcat
  [ methodDecl m
  , lbrace
  , ppBody body
  , rbrace
  ]

ppBody = nest numSpaces . vcat . map statement

statement :: Statement -> Doc
statement (IfStatement expr body) = vcat $ [ "if" <+> parens (ppExpr expr) <+> lbrace
                                           , ppBody body
                                           , rbrace
                                           ]
statement (AssignmentStatement left Equals right) = s $ ppExpr left <+> "=" <+> ppExpr right
statement (ReturnStatement expr) = s $ "return" <+> ppExpr expr

s x = x <> semi

ppExpr (Other o) = text o
ppExpr (VariableExpr v) = text v
ppExpr (PropertyExpr l r) = ppExpr l <> "." <> ppExpr r
ppExpr (NotExpr n) = "!" <+> (parens $ ppExpr n)
ppExpr (MessageExpr obj (SelectorNoArgument sel) []) = brackets (ppExpr obj <+> text sel)
ppExpr (MessageExpr obj (SelectorWithArguments selArgs) args) = brackets (ppExpr obj <+> methodArgs selArgs args)
ppExpr (CastExpr typ expr) = parens (ppType typ) <> ppExpr expr
ppExpr (VarExpr typ expr) = (ppType typ) <+> ppExpr expr
ppExpr (IVarExpr l r) = (ppExpr l) <> "->" <> ppExpr r
ppExpr (OperatorExpr l o r) = ppExpr l <+> text o <+> ppExpr r
ppExpr (AtExpr e) = "@" <> (parens $ ppExpr e)

methodArgs :: [String] -> [Expression] -> Doc
methodArgs [] [] = ""
methodArgs (x:xs) (y:ys) = text x <> ":" <> ppExpr y <+> methodArgs xs ys
methodArgs _ _ = error "method args and selector not matching"

methodName (SimpleName name) = text name
methodName (NameWithArguments arguments) = parameterList arguments

ppSpecifier Class = "+"
ppSpecifier Instance = "-"

parameterList = hsep . map argument

argument (Argument paramName typ variableName) = text paramName <> ":" <> (parens $ ppType typ) <> text variableName

ppType Id = "id"
ppType InstanceType = "instancetype"
ppType Void = "void"
ppType (Pointer p) = ppType p <> "*"
ppType (Scalar s) = text s
ppType (QualifiedWithProtocols t protocols) = ppType t <> angular (commaList $ map text protocols)

angular :: Doc -> Doc
angular x = "<" <> x <> ">"

whitespace :: Doc
whitespace = " "

commaList :: [Doc] -> Doc
commaList = hcat . intersperse (comma <> whitespace)

{-
example = Value "Chris" 
          [ Field (Scalar "NSUInteger") "count"
          , Field (Pointer $ Scalar "NSString") "string"
          , Field (QualifiedWithProtocols Id ["MyDelegate","Copying"]) "delegate"
          ]


initializer fields = instanceMethod "instancetype" fields $
     vcat
     [ ""
     ,  "self = [super init];"
     , "if (self) {"
     ,  nest numSpaces $ vcat (assignments fields)
     , "}"
     , "return self"
     ]


assignments = map (statement . assignment)
 where assignment (Field _ f) = "self." <> text f <> " = " <> text f

statement s = s <> ";"


firstParameter start (Field t name) = parameter (start <> text (capitalize name)) t (text name)


parameter paramName typ variableName = paramName <> ":" <> (parens $ ppType typ) <> variableName

capitalize :: String -> String
capitalize (c:cs) = (C.toUpper c) : cs


-}
