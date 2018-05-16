--Written by Kevin Kazianschütz

main :: IO () --Einstiegspunkt
main = do
  putStrLn ""
  putStrLn "Loading TVRecommender..."
  putStrLn ""
  printHelp
  mainmenu


mainmenu :: IO () --Nimmt Eingabe entgegen und leitet entsprechend weiter
mainmenu = do
  input <- getLine
  case input of
    "help" -> printHelp >> mainmenu
    "exit" -> putStrLn "Thanks for using TVRecommender!"
    _ -> putStrLn ("Command '" ++ input ++ "' is unknown!\n\nPlease enter a command or type 'help' for assistance!") >> mainmenu


printHelp :: IO () --Gibt Liste der möglichen Befehle sowie Aufforderung zur Eingabe aus
printHelp = do
  putStrLn "This Program supports the following commands:"
  putStrLn "\t 'help' ... shows this message"
  putStrLn "\t 'exit' ... terminate the application"
  putStrLn ""
  putStrLn "Please enter a command or type 'help' for assistance!"
