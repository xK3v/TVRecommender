--Written by Kevin Kazianschütz

import Data.List
import Control.Monad
import Control.Monad.IO.Class
import Network.HTTP.Conduit
import Text.HTML.TagSoup
import qualified Data.ByteString.Lazy.Char8 as L8

main :: IO () --Einstiegspunkt
main = do
  putStrLn ""
  putStrLn "Loading TVRecommender..."
  putStrLn ""
  printHelp
  mainmenu
  putStrLn "testing:"
  --getTags
  --L8.putStr =<< simpleHttp "https://www.tele.at/tv-programm/2015-im-tv.html?stationType=-1&start=0&limit=5&format=raw"


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


getTags = do
  site <- simpleHttp "https://www.tele.at/tv-programm/2015-im-tv.html?stationType=-1&start=0&limit=5&format=raw"
  let varia = L8.unpack site
  putStrLn varia
