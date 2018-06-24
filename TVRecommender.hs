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
import Network.HTTP.Conduit --install http-conduit
import Text.XML.HXT.Core --install hxt
--import Text.HandsomeSoup
--import Control.Parallel.Strategies
import qualified Control.Monad.Parallel as PAR --install monad-parallel

--import Codec.Binary.UTF8.String
--import qualified Data.ByteString.Lazy as LBS


main :: IO () --Einstiegspunkt
main = do
  --TODO: Umlaute
  setLocaleEncoding GHC.IO.Encoding.utf8
  putStrLn ""
  putStrLn "Loading TVRecommender..."

  --downloads the information about all the broadcasts:
  info <- parseSite
  printHelp
  mainMenu (return info)


mainMenu :: IO (V.Vector (Int,String,String,String,String,String,[String])) -> IO () --takes input and calls relevant function(s)
mainMenu info = do
  putStrLn "\nPlease enter a command or type 'help' for assistance!"
  input <- getLine
  if null input then mainMenu info else --check if input is empty
    if head (words input)=="show" && (2 == length (words input)) then showBroadcast (read $ head $ tail $ words input) info >> mainMenu info else --check if input is show
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



--two functions to get only every other element of a list:
dropEveryOther :: [a] -> [a] -> [a]
dropEveryOther acc [] = acc
dropEveryOther acc (h:t) = dropEveryOther' (h : acc) t
dropEveryOther' :: [a] -> [a] -> [a]
dropEveryOther' acc [] = acc
dropEveryOther' acc (_:t) = dropEveryOther acc t

parseSite :: IO (V.Vector (Int,String,String,String,String,String,[String]))
parseSite = do
  --downloading website:
  siteString   <- simpleHttp "https://www.tele.at/tv-programm/2015-im-tv.html?stationType=-1&start=0&limit=500&format=raw"
  --let st1 = LBS.unpack siteString
  --let st2 = decode st1
  --let site = readString [withParseHTML yes, withWarnings no] $ decode $ LBS.unpack siteString
  let site = readString [withParseHTML yes, withWarnings no] $ L8.unpack siteString
  --let site = readString [withParseHTML yes, withWarnings no] $ decode $ LBS.unpack siteString

  --filtering the relevant information:
  zeiten         <- runX $ site //> hasAttrValue "class" (isInfixOf "broadcast") //> hasName "strong" >>> deep getText
  sender         <- runX $ site //> hasAttrValue "class" (== "station") //> removeAllWhiteSpace //> deep getText
  sendungen_ws   <- runX $ site //> hasAttrValue "class" (=="title") //> removeAllWhiteSpace /> getText
  genre_ws       <- runX $ site //> hasAttrValue "class" (=="genre") >>> removeAllWhiteSpace >>> deep getText
  link_short     <- runX $ site //> hasAttrValue "class" (== "title") //> hasName "a" >>> getAttrValue "href"

  --doing some work on the information:
  let sendungen = map (filter (/= '\n') . filter (/= '\t')) sendungen_ws
  let genre     = map (filter (/= '\n') . filter (/= '\t')) genre_ws

  --selecting only PrimeTime broadcasts
  let zeitenPT     = dropEveryOther [] zeiten
  let senderPT     = dropEveryOther [] sender
  let sendungenPT  = dropEveryOther [] sendungen
  let genrePT      = dropEveryOther [] genre
  let link_shortPT = dropEveryOther [] link_short

  let linkPT = map (\str -> "https://www.tele.at" ++ str) link_shortPT

  --creating tuple with basic information:
  let zipped = zip5 zeitenPT senderPT sendungenPT genrePT linkPT

  --sorting by station name:
  let sorted = sortOn (\(_,send,_,_,_) -> map toLower send) zipped

  --adding numbering:
  --let numbered = map unFoldTuple $ zip [1..length sendungen + 1] sorted
  let numbered  =  zipWith (curry (\(n,(a,b,c,d,e)) -> (n,a,b,c,d,e))) [1..length sendungen + 1] sorted

  putStrLn "Got Content! Reading broadcast details..."
  --Map der parseDetails function parallel (50% faster)
  detailed      <- PAR.mapM parseDetails numbered
  return $ V.fromList detailed


{-
unFoldTuple :: (t,(t1,t2,t3,t4,t5)) -> (t,t1,t2,t3,t4,t5)
unFoldTuple (n,(a,b,c,d,e)) = (n,a,b,c,d,e)
-}


parseDetails :: (Int,String,String,String,String,String) -> IO (Int,String,String,String,String,String,[String]) --adding text and actors
parseDetails (n,a,b,c,d,link) = do
  --downloading detailed website:
  detailSiteString <- simpleHttp link
  let detailSite    = readString [withParseHTML yes, withWarnings no] $ L8.unpack detailSiteString

  --getting relevant information (text and actors):
  text   <- runX $ detailSite //> hasAttrValue "class" (== "long-text") >>> deep getText
  actors <- runX $ detailSite //> hasAttrValue "class" (== "actor") //> hasName "span" >>> deep getText

  --putting together new tuple and dealing with missing information:
  --let detailBcs = (n,a,b,c,d,if null text then "No information available" else head text,if null actors then ["-"] else map (\s -> '\t':'-':s) actors)
  let detailBcs = (n,a,b,c,d,if null text then "No information available" else head text,if null actors then ["-"] else actors)
  return detailBcs


listBroadcasts :: IO (V.Vector (Int,String,String,String,String,String,[String])) -> IO () --prints overview of todays program
listBroadcasts info = do
  broadcasts <- info

  --adding tuples to one continuous string:
  let addTuple (n,t_zeiten,t_sender,t_sendungen,t_genre,_,_) = printf "%03d." n ++ "\t" ++ t_zeiten ++ "\t" ++ printf "%- 16s" t_sender ++ "\t" ++ t_sendungen ++ ", " ++ t_genre

  --printing list command:
  putStrLn $ unlines $ map addTuple $ V.toList broadcasts
  --mapM_ putStrLn $ map addTuple zipped


showBroadcast :: Int -> IO (V.Vector(Int,String,String,String,String,String,[String])) -> IO () --prints detailed information of a specific broadcast
showBroadcast n info = do
  bclist <- info

  --selecting requested broadcast:
  let bcinfo = bclist ! (n-1)

  --adding tuple to one continuous string:
  let addDetails (_,t_zeit,t_sender,t_sendung,t_genre,t_text,t_actors) = "\nTitle: " ++ t_sendung ++ " (" ++ t_genre ++ ") \n" ++ t_zeit ++ " " ++ t_sender ++ "\n\n" ++ t_text ++ "\n\nActors:\n" ++ unlines (map (\s -> '\t':'-':s) t_actors)

  --printing show command:
  putStrLn $ addDetails bcinfo


readActors :: IO (Set.Set String) --reads actors from txt file and returns them as a set of strings, creates file if necessary
readActors = do
  fileExists <- doesFileExist "actors.txt"
  if fileExists then do
    actors <- readFile "actors.txt"
    --save actors in a Set:
    return $ Set.fromList $ lines actors
  else
    --create file and return empty Set:
    writeFile "actors.txt" "" >> return Set.empty


listActors :: IO () --shows list of all actors in txt file
listActors = do
  putStrLn ""
  actorList <- readActors
  if null actorList
    then putStrLn "You do not have any favourite actors yet. Please use 'add actor' to add one."
    else putStrLn $ unlines $ Set.toAscList actorList


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
  --add all actors except the one being deleted to a new Set:
  --let !newActorList = Set.filter (/=map toLower name) $ Set.map (map toLower) actorList
  let !newActorList = Set.foldl (\acc a -> if map toLower name == map toLower a then acc else Set.insert a acc) Set.empty actorList
  writeFile "actors.txt" $ unlines $ Set.toAscList newActorList

recommend :: IO [(Int, String, String, String, String, String, [String])] -> IO ()
recommend bclIO = do
  --loading favourite actors and list of broadcasts:
  bcl <- bclIO
  favActorsSet <- readActors
  --making favourite actors lowercase so comparision will be case insensitive:
  let favActors = map (map toLower) (Set.toList favActorsSet)
  --let recommendations = filter (\(_,_,_,_,_,_,bcActors) -> foldr (\a acc -> if a `elem` favActors then True else acc) False bcActors ) bcl
  --filter those broadcasts that contain at least one of the favourite actors:
  let recommendations = filter (\(_,_,_,_,_,_,bcActors) -> foldr (\a acc -> (map toLower a `elem` favActors) || acc) False bcActors ) bcl
  --adding tuple to one continuous string and selecting which actors to show:
  let addRecommendation (n,t_zeit,t_sender,t_sendung,t_genre,_,t_actors) = printf "%03d." n ++ " " ++ t_zeit ++ " " ++ printf "%- 16s" t_sender ++ " " ++ t_sendung ++ " (featuring: " ++ intercalate ", " (filter (\a -> map toLower a `elem` favActors) t_actors) ++ "), " ++ t_genre
  --deal with no recommendations:
  if null recommendations then putStrLn "There are no recommendations for you today." else
    putStrLn $ unlines $ map addRecommendation recommendations
