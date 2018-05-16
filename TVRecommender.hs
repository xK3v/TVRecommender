--Written by Kevin Kazianschütz

main :: IO ()
main = do
  putStrLn ""
  putStrLn "Loading TVRecommender..."
  putStrLn ""
  printHelp


printHelp :: IO ()
printHelp = do
  putStrLn "This Program supports the following commands:"
  putStrLn "\t 'help' ... shows this message"
  putStrLn "\t 'exit' ... terminate the application"
  putStrLn ""
  putStrLn "Please enter a command or type 'help' for assistance!"
