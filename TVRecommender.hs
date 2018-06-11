--Written by Kevin Kazianschütz

import GHC.IO.Encoding
import Data.List
import Control.Monad
import Control.Monad.IO.Class
import Network.HTTP.Conduit
--import Text.HTML.TagSoup
import qualified Data.ByteString.Lazy.Char8 as L8

import Text.XML.HXT.Core
import Text.HandsomeSoup

import Text.Printf (printf)


main :: IO () --Einstiegspunkt
main = do
  setLocaleEncoding GHC.IO.Encoding.utf8
  putStrLn ""
  putStrLn "Loading TVRecommender..."
  putStrLn ""
  printHelp
  mainmenu
  putStrLn "end"
  --getTags
  --L8.putStr =<< simpleHttp "https://www.tele.at/tv-programm/2015-im-tv.html?stationType=-1&start=0&limit=5&format=raw"


mainmenu :: IO () --Nimmt Eingabe entgegen und leitet entsprechend weiter
mainmenu = do
  input <- getLine
  case input of
    "tags" -> getTags
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

{-
getTags = do
  site <- simpleHttp "https://www.tele.at/tv-programm/2015-im-tv.html?stationType=-1&start=0&limit=5&format=raw"
  let varia = parseTags $ L8.unpack site
  --let gettest = innerText . take 2 . dropWhile (~/= "<div class=\"station\">")
  --let testtext = gettest $ parseTags varia --where
  --let broadcasts = map f $ sections (~== TagOpen "div" [("class","genre")]) varia
  --putStrLn $ unlines broadcasts
  --where
  --  f xs = fromTagText (xs !! 2)
  putStrLn $ renderTags $ drop 5 $ take 6 varia
-}
getTags :: IO ()
getTags = do
  site <- simpleHttp "https://www.tele.at/tv-programm/2015-im-tv.html?stationType=-1&start=0&limit=5&format=raw"
  let parsed = readString [withParseHTML yes, withWarnings no] $ L8.unpack site
  --let parsed2 = fromUrl "https://www.tele.at/tv-programm/2015-im-tv.html?stationType=-1&start=0&limit=5&format=raw"
  --sender <- runX $ parsed //> hasAttrValue "class" (== "station") >>> getAttrValue "title"
  sender <- runX $ parsed //> hasAttrValue "class" (== "station") >>> removeAllWhiteSpace /> deep getText
  zeiten <- runX $ parsed //> hasAttrValue "class" (isInfixOf "broadcast") >>> getChildren //> hasName "strong" >>> deep getText
  sendungen_ws <- runX $ parsed //> hasAttrValue "class" (=="title") >>> getChildren >>> removeAllWhiteSpace /> getText
  --sendungen <- runX $ parsed //> hasAttrValue "class" (=="bc-item") //> hasAttrValue "class" (=="title") >>> getChildren >>> removeAllWhiteSpace /> getText
  -- TODO: nur erste sendung jedes "bc-item" nehmen
  let sendungen = map (filter (/= '\n') . filter (/= '\t')) sendungen_ws
  let zipped = zip4 [1..length sendungen + 1] zeiten sender sendungen
  let addTuple (n,t_zeiten,t_sender,t_sendungen) = printf "%03d." n ++ "\t" ++ t_zeiten ++ "\t" ++ printf "%- 16s" t_sender ++ "\t" ++ t_sendungen
  mapM_ putStrLn sender
  putStrLn ""
  mapM_ putStrLn zeiten
  putStrLn ""
  mapM_ putStrLn sendungen
  putStrLn ""
  mapM_ putStrLn $ map addTuple zipped

{-
--programList :: Num a => [String] -> [String] -> [String] -> [(a,String,String,String)]
programList zeiten sender sendungen = programListRec 1 zeiten sender sendungen [] where
  programListRec _ [] [] [] list = list
  programListRec n (hz:tz) (hs:ts) (hf:tf) list = programListRec (n+1) tz ts tf ((n,hz,hs,hf) : list)
-}
