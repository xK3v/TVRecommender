--Written by Kevin Kazianschütz

import System.IO
import System.Directory

main :: IO () --Einstiegspunkt
main = do
  putStrLn "\nLoading TVRecommender..."
  printHelp
  mainMenu


mainMenu :: IO () --Nimmt Eingabe entgegen und leitet entsprechend weiter
mainMenu = do
  putStrLn "\nPlease enter a command or type 'help' for assistance!"
  input <- getLine
  case input of
    "list actors" -> getActors >> mainMenu
    "help" -> printHelp >> mainMenu
    "exit" -> putStrLn "Thanks for using TVRecommender!"
    _ -> putStrLn ("Command '" ++ input ++ "' is unknown!") >> mainMenu


printHelp :: IO () --Gibt Liste der möglichen Befehle sowie Aufforderung zur Eingabe aus
printHelp = do
  putStrLn "\nThis Program supports the following commands:"
  putStrLn "\t 'list actors' ... shows a list of all your favourite actors"
  putStrLn "\t 'help' ... shows this message"
  putStrLn "\t 'exit' ... terminate the application"

getActors :: IO ()
getActors = do
  fileExists <- doesFileExist "actors.txt"
  if fileExists then do
    putStrLn ""
    handle <- openFile "actors.txt" ReadMode
    actorText <- hGetContents handle
    putStrLn actorText
    hClose handle
  else putStrLn "\nFile 'actors.txt' does not exist yet. Create it by adding your first actor."
