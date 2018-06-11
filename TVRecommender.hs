--Written by Kevin Kazianschütz

{-# LANGUAGE BangPatterns #-}

import GHC.IO.Encoding
import Data.List
import Data.Char
--import Control.Monad
--import Control.Monad.IO.Class
import Network.HTTP.Conduit
--import Text.HTML.TagSoup
import qualified Data.ByteString.Lazy.Char8 as L8

import Text.XML.HXT.Core
import Text.HandsomeSoup

import Text.Printf (printf)

import System.Directory


main :: IO () --Einstiegspunkt
main = do
  setLocaleEncoding GHC.IO.Encoding.utf8
  putStrLn ""
  putStrLn "Loading TVRecommender..."
  putStrLn ""
  printHelp
  mainMenu


mainMenu :: IO () --takes input and calls relevant function(s)
mainMenu = do
  putStrLn "\nPlease enter a command or type 'help' for assistance!"
  input <- getLine
  case map toLower $ unwords $ take 2 $ words input of
    "list" -> getTags >> mainMenu
    "add actor" -> addActor (unwords $ drop 2 $ words input) >> mainMenu
    "list actors" -> listActors >> mainMenu
    "delete actor" -> removeActor (unwords $ drop 2 $ words input) >> mainMenu
    "help" -> printHelp >> mainMenu

    "exit" -> putStrLn "Thanks for using TVRecommender!"
    _ -> putStrLn ("Command '" ++ input ++ "' is unknown!") >> mainMenu


printHelp :: IO () --show list of all the possible commands
printHelp = do
  putStrLn "\nThis Program supports the following commands:"
  putStrLn "\t 'list' ... shows an overview of all broadcasts"
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

--TODO: check why 'avoid lambda'? DONE?!
removeActor :: String -> IO () --removes an actor from txt file
removeActor name = do
  actorList <- readActors
  --let !newActorList = filter (/=map toLower name) $ map (\n -> map toLower n) actorList --BangPatterns needed because lazy evaluation produces an IO error here
  let !newActorList = filter (/=map toLower name) $ map (map toLower) actorList --BangPatterns needed because lazy evaluation produces an IO error here
  --let !newActorList = [map toLower x | x <- actorList, x/= map toLower name]
  writeFile "actors.txt" $ unlines newActorList

getTags :: IO ()
getTags = do
  site <- simpleHttp "https://www.tele.at/tv-programm/2015-im-tv.html?stationType=-1&start=0&limit=5&format=raw"
  let parsed = readString [withParseHTML yes, withWarnings no] $ L8.unpack site
  --sender <- runX $ parsed //> hasAttrValue "class" (== "station") >>> getAttrValue "title"
  sender <- runX $ parsed //> hasAttrValue "class" (== "station") >>> removeAllWhiteSpace /> deep getText
  zeiten <- runX $ parsed //> hasAttrValue "class" (isInfixOf "broadcast") >>> getChildren //> hasName "strong" >>> deep getText
  sendungen_ws <- runX $ parsed //> hasAttrValue "class" (=="title") >>> getChildren >>> removeAllWhiteSpace /> getText
  genre_ws <- runX $ parsed //> hasAttrValue "class" (=="genre") >>> removeAllWhiteSpace >>> deep getText
  --sendungen <- runX $ parsed //> hasAttrValue "class" (=="bc-item") //> hasAttrValue "class" (=="title") >>> getChildren >>> removeAllWhiteSpace /> getText
  -- TODO: nur erste sendung jedes "bc-item" nehmen
  let sendungen = map (filter (/= '\n') . filter (/= '\t')) sendungen_ws
  let genre = map (filter (/= '\n') . filter (/= '\t')) genre_ws
  let zipped = zip5 [1..length sendungen + 1] zeiten sender sendungen genre
  let addTuple (n,t_zeiten,t_sender,t_sendungen,t_genre) = printf "%03d." n ++ "\t" ++ t_zeiten ++ "\t" ++ printf "%- 16s" t_sender ++ "\t" ++ t_sendungen ++ ", " ++ t_genre
  --mapM_ putStrLn genre

  --mapM_ putStrLn $ map addTuple zipped
  putStrLn $ unlines $ map addTuple zipped
