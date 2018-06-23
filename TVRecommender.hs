--Written by Kevin Kazianschütz

{-# LANGUAGE BangPatterns #-}

import GHC.IO.Encoding
import Data.List
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as L8
import Text.Printf (printf)
import System.Directory
import qualified Data.Vector as V
import Data.Vector ((!))
import qualified Data.Set as Set

--import Control.Monad
--import Control.Monad.IO.Class

--These have to be installed first:
import Network.HTTP.Conduit
import Text.XML.HXT.Core
--import Text.HandsomeSoup
--import Text.HTML.TagSoup
--import Control.Parallel.Strategies
import qualified Control.Monad.Parallel as PAR



main :: IO () --Einstiegspunkt
main = do
  --TODO: Umlaute
  setLocaleEncoding GHC.IO.Encoding.utf8
  putStrLn ""
  putStrLn "Loading TVRecommender..."
  let info = parseSite --Downloads the information about all the broadcasts
  --putStrLn "Got Content!"
  printHelp
  mainMenu info


mainMenu :: IO (V.Vector (Int,String,String,String,String,String,[String])) -> IO () --takes input and calls relevant function(s)
mainMenu info = do
  putStrLn "\nPlease enter a command or type 'help' for assistance!"
  input <- getLine
  if null input then mainMenu info else
    if head (words input)=="show" then showBroadcast (read $ head $ tail $ words input) info >> mainMenu info else
      case map toLower $ unwords $ take 2 $ words input of
        "list"         -> listBroadcasts info >> mainMenu info
        "add actor"    -> addActor (unwords $ drop 2 $ words input) >> mainMenu info
        "list actors"  -> listActors >> mainMenu info
        "delete actor" -> removeActor (unwords $ drop 2 $ words input) >> mainMenu info
        "recommend"    -> recommend (fmap V.toList info) >> mainMenu info
        "help"         -> printHelp >> mainMenu info
        "exit"         -> putStrLn "Thanks for using TVRecommender!"
        _              -> putStrLn ("Command '" ++ input ++ "' is unknown!") >> mainMenu info


printHelp :: IO () --show list of all the possible commands
printHelp = do
  putStrLn "\nThis program supports the following commands:"
  putStrLn "\t 'list' ... shows an overview of all broadcasts"
  putStrLn "\t 'show' n ... shows the details of the n-th entry in the program list"
  putStrLn "\t 'add actor' name ... add a given name to your list of favourite actors"
  putStrLn "\t 'list actors' ... shows a list of all your favourite actors"
  putStrLn "\t 'delete actor' name ... removes the given name from your list of favourite actors"
  putStrLn "\t 'recommend' ... shows a list of broadcasts featuring your favourite actors"
  putStrLn "\t 'help' ... shows this message"
  putStrLn "\t 'exit' ... terminate the application"

--TODO: nur Primetime
parseSite :: IO (V.Vector (Int,String,String,String,String,String,[String]))
parseSite = do
  --downloading website:
  siteString   <- simpleHttp "https://www.tele.at/tv-programm/2015-im-tv.html?stationType=-1&start=0&limit=500&format=raw"
  let site     = readString [withParseHTML yes, withWarnings no] $ L8.unpack siteString

  --filtering the relevant information:
  sender         <- runX $ site //> hasAttrValue "class" (== "station") //> removeAllWhiteSpace //> deep getText
  zeiten         <- runX $ site //> hasAttrValue "class" (isInfixOf "broadcast") //> hasName "strong" >>> deep getText
  sendungen_ws   <- runX $ site //> hasAttrValue "class" (=="title") //> removeAllWhiteSpace /> getText
  genre_ws       <- runX $ site //> hasAttrValue "class" (=="genre") >>> removeAllWhiteSpace >>> deep getText
  link_short     <- runX $ site //> hasAttrValue "class" (== "title") //> hasName "a" >>> getAttrValue "href"

  --doing some work on the information:
  let sendungen = map (filter (/= '\n') . filter (/= '\t')) sendungen_ws
  let genre     = map (filter (/= '\n') . filter (/= '\t')) genre_ws
  let link      = map (\str -> "https://www.tele.at" ++ str) link_short

  --creating tuple with basic information
  let zipped    = zip5 zeiten sender sendungen genre link

  --sorting by station name
  let sorted    = sortOn (\(_,send,_,_,_) -> map toLower send) zipped

  --adding numbering
  --let numbered = map unFoldTuple $ zip [1..length sendungen + 1] sorted
  let numbered  =  zipWith (curry (\(n,(a,b,c,d,e)) -> (n,a,b,c,d,e))) [1..length sendungen + 1] sorted
  detailed      <- PAR.mapM parseDetails numbered --Map der parseDetails function
  return $ V.fromList detailed


{-
unFoldTuple :: (t,(t1,t2,t3,t4,t5)) -> (t,t1,t2,t3,t4,t5)
unFoldTuple (n,(a,b,c,d,e)) = (n,a,b,c,d,e)
-}


parseDetails :: (Int,String,String,String,String,String) -> IO (Int,String,String,String,String,String,[String]) --adding text and actors
parseDetails (n,a,b,c,d,link) = do
  --downloading detailed website
  detailSiteString <- simpleHttp link
  let detailSite = readString [withParseHTML yes, withWarnings no] $ L8.unpack detailSiteString

  --getting relevant information (text and actors)
  text   <- runX $ detailSite //> hasAttrValue "class" (== "long-text") >>> deep getText
  actors <- runX $ detailSite //> hasAttrValue "class" (== "actor") //> hasName "span" >>> deep getText

  --putting together new tuple and dealing with missing information
  --let detailBcs = (n,a,b,c,d,if null text then "No information available" else head text,if null actors then ["-"] else map (\s -> '\t':'-':s) actors)
  let detailBcs = (n,a,b,c,d,if null text then "No information available" else head text,if null actors then ["-"] else actors)
  return detailBcs


listBroadcasts :: IO (V.Vector (Int,String,String,String,String,String,[String])) -> IO () --prints overview of todays program
listBroadcasts info = do
  broadcasts <- info

  --adding tuples to one continuous string
  let addTuple (n,t_zeiten,t_sender,t_sendungen,t_genre,_,_) = printf "%03d." n ++ "\t" ++ t_zeiten ++ "\t" ++ printf "%- 16s" t_sender ++ "\t" ++ t_sendungen ++ ", " ++ t_genre
  --printing list command
  putStrLn $ unlines $ map addTuple $ V.toList broadcasts
  --mapM_ putStrLn $ map addTuple zipped


showBroadcast :: Int -> IO (V.Vector(Int,String,String,String,String,String,[String])) -> IO () --prints detailed information of a specific broadcast
showBroadcast n info = do
  bclist <- info

  let bcinfo = bclist ! (n-1)
  let addDetails (_,t_zeit,t_sender,t_sendung,t_genre,t_text,t_actors) = "\nTitle: " ++ t_sendung ++ " (" ++ t_genre ++ ") \n" ++ t_zeit ++ " " ++ t_sender ++ "\n\n" ++ t_text ++ "\n\nActors:\n" ++ unlines (map (\s -> '\t':'-':s) t_actors)
  putStrLn $ addDetails bcinfo


readActors :: IO (Set.Set String) --reads actors from txt file and returns them as a set of strings, creates file if necessary
readActors = do
  fileExists <- doesFileExist "actors.txt"
  if fileExists then do
    actors <- readFile "actors.txt"
    return $ Set.fromList $ lines actors
  else
    writeFile "actors.txt" "" >> return Set.empty


listActors :: IO () --shows list of all actors in txt file
listActors = do
  putStrLn ""
  actorList <- readActors
  putStrLn $ unlines $ Set.toAscList actorList


--TODO: check for any upper/lowercase variants before adding new actor
addActor :: String -> IO () --adds a new actor to txt file
addActor name = do
  actorList <- readActors
  --BangPatterns needed because lazy evaluation produces an IO error here
  let !cleanActorList = Set.filter (/="") actorList --remove empty lines
  let newActorList = Set.insert name cleanActorList --insert actor
  writeFile "actors.txt" $ unlines $ Set.toAscList newActorList


removeActor :: String -> IO () --removes an actor from txt file
removeActor name = do
  actorList <- readActors
  --BangPatterns needed because lazy evaluation produces an IO error here
  --let !newActorList = Set.filter (/=map toLower name) $ Set.map (map toLower) actorList
  let !newActorList = Set.foldl (\acc a -> if map toLower name == map toLower a then acc else Set.insert a acc) Set.empty actorList
  writeFile "actors.txt" $ unlines $ Set.toAscList newActorList

recommend :: IO [(Int, String, String, String, String, String, [String])] -> IO ()
recommend bclIO = do
  bcl <- bclIO
  favActorsSet <- readActors
  let favActors = Set.toList favActorsSet
  --let recommendations = filter (\(_,_,_,_,_,_,bcActors) -> foldr (\a acc -> if a `elem` favActors then True else acc) False bcActors ) bcl
  let recommendations = filter (\(_,_,_,_,_,_,bcActors) -> foldr (\a acc -> (a `elem` favActors) || acc) False bcActors ) bcl
  let addRecommendation (n,t_zeit,t_sender,t_sendung,t_genre,_,t_actors) = printf "%03d." n ++ " " ++ t_zeit ++ " " ++ t_sender ++ " " ++ t_sendung ++ " (featuring: " ++ intercalate ", " (filter (`elem` favActors) t_actors) ++ "), " ++ t_genre
  if null recommendations then putStrLn "There are no recommendations for you today." else
    putStrLn $ unlines $ map addRecommendation recommendations
