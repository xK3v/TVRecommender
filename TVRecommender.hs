--Written by Kevin Kazianschütz

import System.IO
import System.Directory
import Data.List

main :: IO () --Einstiegspunkt
main = do
  putStrLn "\nLoading TVRecommender..."
  printHelp
  mainMenu


mainMenu :: IO () --Nimmt Eingabe entgegen und leitet entsprechend weiter
mainMenu = do
  putStrLn "\nPlease enter a command or type 'help' for assistance!"
  input <- getLine
  case unwords $ take 2 $ words input of
    "add actor" -> addActor (unwords $ drop 2 $ words input) >> mainMenu
    "list actors" -> getActors >> mainMenu
    "help" -> printHelp >> mainMenu
    "exit" -> putStrLn "Thanks for using TVRecommender!"
    _ -> putStrLn ("Command '" ++ input ++ "' is unknown!") >> mainMenu


printHelp :: IO () --Gibt Liste der möglichen Befehle sowie Aufforderung zur Eingabe aus
printHelp = do
  putStrLn "\nThis Program supports the following commands:"
  putStrLn "\t 'add actor' ... add a given name to your list of favourite actors"
  putStrLn "\t 'list actors' ... shows a list of all your favourite actors"
  putStrLn "\t 'help' ... shows this message"
  putStrLn "\t 'exit' ... terminate the application"


readActors :: IO [String]
readActors = do
  actors <- readFile "actors.txt"
  --let actorsClean = filter (/="") $ lines actors
  return $ sort $filter (/="") $ lines actors


getActors :: IO ()
getActors = do
  fileExists <- doesFileExist "actors.txt"
  if fileExists then do
    putStrLn ""
    --handle <- openFile "actors.txt" ReadMode
    --actorText <- hGetContents handle
    actorList <- readActors
    putStrLn $ unlines actorList
    --hClose handle
  else putStrLn "\nFile 'actors.txt' does not exist yet. Create it by adding your first actor."


addActor :: String -> IO ()
addActor name = do
  fileExists <- doesFileExist "actors.txt"
  if fileExists then do
    --actors <- readFile "actors.txt"
    --let actorList = lines actors
    actorList <- readActors
    if name `notElem` actorList then appendFile "actors.txt" ("\n" ++ name)
    else putStr ""
  else writeFile "actors.txt" name


removeActor :: String -> IO ()
removeActor name = do
  fileExists <- doesFileExist "actors.txt"
  putStrLn ""
