import Values

testEmptyDeclaration = parseString "value {}" == Value "" []
testNameDeclaration = parseString "value List {}" == Value "List" []

allTests = [ testEmptyDeclaration
           , testNameDeclaration
           ]
