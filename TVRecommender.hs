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
    "list actors" -> listActors >> mainMenu
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
  fileExists <- doesFileExist "actors.txt"
  if fileExists then do
    actors <- readFile "actors.txt"
    return $ sort $ filter (/="") $ lines actors
  else
    writeFile "actors.txt" "" >> return []


listActors :: IO ()
listActors = do
  putStrLn ""
  actorList <- readActors
  putStrLn $ unlines actorList


addActor :: String -> IO ()
addActor name = do
  actorList <- readActors
  if name `notElem` actorList then do
    let newActorList = insert name actorList
    writeFile "actors.txt" $ unlines newActorList
  else
    writeFile "actors.txt" $ unlines actorList


removeActor :: String -> IO ()
removeActor name = do
  putStrLn ""
