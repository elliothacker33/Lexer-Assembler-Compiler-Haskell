
import Test
import System.Exit (exitSuccess)

main :: IO ()
main = do
  runAssemblerTests
  runParserTests


custom_main :: IO()
custom_main = do

    putStrLn "Choose an option:"
    putStrLn "1. Run tests"
    putStrLn "2. Run custom input for Parser"
    putStrLn "3. Quit"
    
    option <- getLine
    
    case option of
        "1" -> do
            runAssemblerTests
            runParserTests
            custom_main
        "2" -> do
            putStrLn "Enter custom input for Parser:"
            input <- getLine
            let result = testParser input
            putStrLn $ "Result: " ++ show result
            custom_main
        "3" -> exitSuccess
