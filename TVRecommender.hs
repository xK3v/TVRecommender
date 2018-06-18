--Written by Kevin Kazianschütz

{-# LANGUAGE BangPatterns #-}

import GHC.IO.Encoding
import Data.List
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as L8
import Text.Printf (printf)
import System.Directory
--import Control.Monad
--import Control.Monad.IO.Class

--These have to be installed first:
import Network.HTTP.Conduit
import Text.XML.HXT.Core
--import Text.HandsomeSoup
--import Text.HTML.TagSoup

main :: IO () --Einstiegspunkt
main = do
  setLocaleEncoding GHC.IO.Encoding.utf8
  putStrLn ""
  putStrLn "Loading TVRecommender..."
  let !info = parseSite
  putStrLn "Got Content!"
  printHelp
  mainMenu info


mainMenu :: IO [(Int,String,String,String,String,String)] -> IO () --takes input and calls relevant function(s)
mainMenu info = do
  putStrLn "\nPlease enter a command or type 'help' for assistance!"
  input <- getLine
  if head (words input)=="show" then printHelp else
    case map toLower $ unwords $ take 2 $ words input of
      "list" -> listBroadcasts info >> mainMenu info
      "add actor" -> addActor (unwords $ drop 2 $ words input) >> mainMenu info
      "list actors" -> listActors >> mainMenu info
      "delete actor" -> removeActor (unwords $ drop 2 $ words input) >> mainMenu info
      "help" -> printHelp >> mainMenu info
      "exit" -> putStrLn "Thanks for using TVRecommender!"
      _ -> putStrLn ("Command '" ++ input ++ "' is unknown!") >> mainMenu info


printHelp :: IO () --show list of all the possible commands
printHelp = do
  putStrLn "\nThis Program supports the following commands:"
  putStrLn "\t 'list' ... shows an overview of all broadcasts"
  putStrLn "\t 'add actor' name ... add a given name to your list of favourite actors"
  putStrLn "\t 'list actors' ... shows a list of all your favourite actors"
  putStrLn "\t 'delete actor' name ... removes the given name from your list of favourite actors"
  putStrLn "\t 'help' ... shows this message"
  putStrLn "\t 'exit' ... terminate the application"

parseSite :: IO [(Int,String,String,String,String,String)]
parseSite = do
  site <- simpleHttp "https://www.tele.at/tv-programm/2015-im-tv.html?stationType=-1&start=0&limit=500&format=raw"
  let parsed = readString [withParseHTML yes, withWarnings no] $ L8.unpack site
  sender <- runX $ parsed //> hasAttrValue "class" (== "station") >>> removeAllWhiteSpace /> deep getText
  zeiten <- runX $ parsed //> hasAttrValue "class" (isInfixOf "broadcast") >>> getChildren //> hasName "strong" >>> deep getText
  sendungen_ws <- runX $ parsed //> hasAttrValue "class" (=="title") >>> getChildren >>> removeAllWhiteSpace /> getText
  genre_ws <- runX $ parsed //> hasAttrValue "class" (=="genre") >>> removeAllWhiteSpace >>> deep getText
  link_short <- runX $ parsed //> hasAttrValue "class" (== "title") >>> getChildren >>> hasName "a" >>> getAttrValue "href"
  --sendungen <- runX $ parsed //> hasAttrValue "class" (=="bc-item") //> hasAttrValue "class" (=="title") >>> getChildren >>> removeAllWhiteSpace /> getText
  -- TODO: nur erste sendung jedes "bc-item" nehmen
  let sendungen = map (filter (/= '\n') . filter (/= '\t')) sendungen_ws
  let genre = map (filter (/= '\n') . filter (/= '\t')) genre_ws
  let link = map (\str -> "https://www.tele.at" ++ str) link_short
  let zipped = zip5 zeiten sender sendungen genre link
  --let zipped = zip6 [1..length sendungen + 1] zeiten sender sendungen genre link
  let sorted = sortOn (\(_,s,_,_,_) -> map toLower s) zipped
  --let numbered = map unFoldTuple $ zip [1..length sendungen + 1] sorted
  --let numbered = zipWith (curry unFoldTuple) [1..length sendungen + 1] sorted
  let numbered = zipWith (curry (\(n,(a,b,c,d,e)) -> (n,a,b,c,d,e))) [1..length sendungen + 1] sorted
  let test = map parseDetails numbered -- TODO: Liste von tuples mit allen relevanten informationen
  return numbered
{-
unFoldTuple :: (t,(t1,t2,t3,t4,t5)) -> (t,t1,t2,t3,t4,t5)
unFoldTuple (n,(a,b,c,d,e)) = (n,a,b,c,d,e)
-}

parseDetails :: (Int,String,String,String,String,String) -> IO (Int,String,String,String,String,String,[String])
parseDetails (n,a,b,c,d,link) = do
  detailSite <- simpleHttp link
  let detailsParsed = readString [withParseHTML yes, withWarnings no] $ L8.unpack detailSite
  text <- runX $ detailsParsed //> hasAttrValue "class" (== "long-text") >>> deep getText
  actors <- runX $ detailsParsed //> hasAttrValue "class" (== "actor") >>> getChildren >>> hasName "span" >>> deep getText
  let detailBcs = (n,a,b,c,d,head text,actors)
  return detailBcs


listBroadcasts :: IO [(Int,String,String,String,String,String)] -> IO ()
listBroadcasts info = do
  let addTuple (n,t_zeiten,t_sender,t_sendungen,t_genre,_) = printf "%03d." n ++ "\t" ++ t_zeiten ++ "\t" ++ printf "%- 16s" t_sender ++ "\t" ++ t_sendungen ++ ", " ++ t_genre
  broadcasts <- info
  putStrLn $ unlines $ map addTuple broadcasts
  --mapM_ putStrLn $ map addTuple zipped


readActors :: IO [String] --reads actors from txt file and returns them as a list of strings, creates file if necessary
readActors = do
  fileExists <- doesFileExist "actors.txt"
  if fileExists then do
    actors <- readFile "actors.txt"
    --return $ sort $ filter (/="") $ lines actors --sort list of actors, in case actors have been inserted manually
    return $ sortBy (\l r -> map toLower l `compare` map toLower r) $ filter (/="") $ lines actors --sort list of actors, in case actors have been inserted manually
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
