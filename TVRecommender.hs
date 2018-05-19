--Written by Kevin Kazianschütz

{-# LANGUAGE BangPatterns #-}


--import System.IO
import System.Directory
import Data.List
import Data.Char

main :: IO () --Entry point
main = do
  putStrLn "\nLoading TVRecommender..."
  printHelp
  mainMenu


mainMenu :: IO () --takes input and calls relevant function(s)
mainMenu = do
  putStrLn "\nPlease enter a command or type 'help' for assistance!"
  input <- getLine
  case map toLower $ unwords $ take 2 $ words input of
    "add actor" -> addActor (unwords $ drop 2 $ words input) >> mainMenu
    "list actors" -> listActors >> mainMenu
    "delete actor" -> removeActor (unwords $ drop 2 $ words input) >> mainMenu
    "help" -> printHelp >> mainMenu
    "exit" -> putStrLn "Thanks for using TVRecommender!"
    _ -> putStrLn ("Command '" ++ input ++ "' is unknown!") >> mainMenu


printHelp :: IO () --show list of all the possible commands
printHelp = do
  putStrLn "\nThis Program supports the following commands:"
  putStrLn "\t 'add actor' name ... add a given name to your list of favourite actors"
  putStrLn "\t 'list actors' ... shows a list of all your favourite actors"
  putStrLn "\t 'delete actor' name ... removes the given name from your list of favourite actors"
  putStrLn "\t 'help' ... shows this message"
  putStrLn "\t 'exit' ... terminate the application"


--TODO: sort independently of upper/lower case
readActors :: IO [String] --reads actors from txt file and returns them as a list of strings, creates file if necessary
readActors = do
  fileExists <- doesFileExist "actors.txt"
  if fileExists then do
    actors <- readFile "actors.txt"
    return $ sort $ filter (/="") $ lines actors --sort list of actors, in case actors have been inserted manually
  else
    writeFile "actors.txt" "" >> return []


listActors :: IO () --shows list of all actors in txt file
listActors = do
  putStrLn ""
  actorList <- readActors
  putStrLn $ unlines actorList


--TODO: check for any upper/lowercase variants before adding new actor
addActor :: String -> IO () --adds a new actor to txt file
addActor name = do
  actorList <- readActors
  let cleanActorList = filter (/="") actorList --remove empty lines
  if name `notElem` cleanActorList then do
    let newActorList = insert name cleanActorList  --insert actor
    writeFile "actors.txt" $ unlines newActorList
  else
    writeFile "actors.txt" $ unlines cleanActorList

--TODO: check why 'avoid lambda'?
removeActor :: String -> IO () --removes an actor from txt file
removeActor name = do
  actorList <- readActors
  let !newActorList = filter (/=map toLower name) $ map (\n -> map toLower n) actorList --BangPatterns needed because lazy evaluation produces an IO error here
  --let !newActorList = [map toLower x | x <- actorList, x/= map toLower name]
  writeFile "actors.txt" $ unlines newActorList
