module Test where
import Compiler
import Parser
import Stack
import Lexer


-- Test assembler function
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
    where (_,stack,state) = run (code,createEmptyStack,createEmptyState)


runAssemblerTests :: IO()
runAssemblerTests = do 
    print (testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10",""))
    print (testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True"))
    print (testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False"))
    print (testAssembler [Push (-20),Tru,Fals] == ("False,True,-20",""))
    print (testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20",""))
    print (testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20",""))
    print (testAssembler [Push (-20),Push (-21), Le] == ("True",""))
    print (testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4"))
    print (testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store"fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1"))


-- Test parser function
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
    where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

runParserTests :: IO()
runParserTests = do
    print (testParser "x := 5; x := x - 1;" == ("","x=4"))
    print (testParser "x := 0 - 2;" == ("","x=-2"))
    print (testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2"))
    print (testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1"))
    print (testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2"))
    print (testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4"))
    print (testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68"))
    print (testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34"))
    print (testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1"))
    print (testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2"))
    print (testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6"))
    print (testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1"))