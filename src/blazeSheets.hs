{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import Text.ParserCombinators.Parsec as P
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8 as R
import qualified Data.ByteString.Lazy as L
import System.Environment
import Data.Map as D
import MusicSheet

--For keeping track of data maybe? Or if we could learn to do w/ monads
--The first int is the Y position, second is X position, and the String
--represents the flats and sharps in the key signature
data Manager = M Int Int String
type NoteMap = Map Float Int


initX = 0
initY = 0
sizeOfStanza = 0
halfOfStanza = 700
sizeOfLetter = 12

setupString :: String
setupString = "#container{height:2300px;width:3000px;position:relative;}" ++
			  "#title{z-index:100;position:absolute;}"++
			  "#image{height:50%;width:50%;position:absolute;}"++
			  "#fourth{z-index:100;position:absolute;height: 50px;width: 50px;}"++
			  "#eighth{z-index:100;position:absolute;height: 50px;width: 50px;}"++
			  "#sixteen{z-index:100;position:absolute;height: 50px;width: 50px;}"++
			  "#whole{z-index:100;position:absolute;height: 50px;width: 50px;}"++
			  "#half{z-index:100;position:absolute;height: 50px;width: 50px;}"++
			  "#wholerest{z-index:100;position:absolute;height: 50px;width: 50px;}"++
			  "#halfrest{z-index:100;position:absolute;height: 50px;width: 50px;}"++
			  "#forth-rest{z-index:100;position:absolute;height: 50px;width: 50px;}"++
			  "#eightrest{z-index:100;position:absolute;height: 50px;width: 50px;}"++
			  "#flag{z-index:100;position:absolute;height: 50px;width: 50px;}"++
			  "#natural{z-index:100;position:absolute;height: 60px;width: 50px;}"++
			  "#sharp{z-index:100;position:absolute;height: 60px;width: 50px;}"++
			  "#commontime{z-index:100;position:absolute;height: 50px;width: 50px;}"++
			  "#beight{z-index:100;position:absolute;height: 50px;width: 50px;}" ++
			  "#line{z-index:100;position:absolute;height: 50px;width: 50px;}"

sheetHtml :: Int -> Int -> Html
sheetHtml a b = H.img H.! A.style (toValue(str)) H.! A.id "image" H.! A.src "img/newSheet.png"
    		 where str = "top:" ++ show a ++ "px; left:" ++ show b ++ "px;"


titleHtml :: String -> Html
titleHtml xs =  H.h1 H.! A.style (toValue("top:0px; left:" ++ show a ++ "px;")) H.! A.id "title" $ toHtml xs
				where a = halfOfStanza - (0.5*(fromIntegral(length xs))*sizeOfLetter)

unitHtml :: String -> Int -> Int -> Html
unitHtml x a b
    | x == "fourth"      = H.img H.! A.style (toValue(str)) H.! A.id "fourth" H.! A.src "img/4th-note.png"
    | x == "eighth"      = H.img H.! A.style (toValue(str)) H.! A.id "eighth" H.! A.src "img/8th-note.png"
    | x == "sixteen"     = H.img H.! A.style (toValue(str)) H.! A.id "sixteen" H.! A.src "img/16th-note.png"
    | x == "whole"       = H.img H.! A.style (toValue(str)) H.! A.id "whole" H.! A.src "img/whole-note.png"
    | x == "half"        = H.img H.! A.style (toValue(str)) H.! A.id "half" H.! A.src "img/half-note.png"
    | x == "wholerest"   = H.img H.! A.style (toValue(str)) H.! A.id "wholerest" H.! A.src "img/wholerest.png"
    | x == "halfrest"    = H.img H.! A.style (toValue(str)) H.! A.id "halfrest" H.! A.src "img/halfrest.png"
    | x == "forth-rest"  = H.img H.! A.style (toValue(str)) H.! A.id "forth-rest" H.! A.src "img/4th-rest.png"
    | x == "eightrest"   = H.img H.! A.style (toValue(str)) H.! A.id "eightrest" H.! A.src "img/eightrest.png"
    | x == "flag"        = H.img H.! A.style (toValue(str)) H.! A.id "flag" H.! A.src "img/flag.png"
    | x == "natural"     = H.img H.! A.style (toValue(str)) H.! A.id "natural" H.! A.src "img/natural.png"
    | x == "sharp"       = H.img H.! A.style (toValue(str)) H.! A.id "sharp" H.! A.src "img/sharp.png"
    | x == "commontime"  = H.img H.! A.style (toValue(str)) H.! A.id "commontime" H.! A.src "img/commontime.png"
    | x == "line"        = H.img H.! A.style (toValue(str)) H.! A.id "line" H.! A.src "img/line.png"
    where str = "top:" ++ show a ++ "px; left:" ++ show b ++ "px;"


--unfinished, will need more parameters for flat/sharp checking?
{-soundToHtml :: Sound -> Int -> Int -> Html
soundToHtml s y x = case s of
	(Note None n d o) -> unitHtml (findUnitType d n) (yPos n o y) (xPos d x)
	(Note Flat n d o) -> do (makeFlat);
							unitHtml (findUnitType d n) (yPos n o y) (xPos d x)
	(Note Sharp n d o) -> do (makesharp);
							 unitHtml (findUnitType d n) (yPos n o y) (xPos d x)
-}
--for testing
test :: String -> Html
test x = docTypeHtml $ do
	H.head $ do
		H.meta H.! A.charset "uft-8"
		H.style $ toHtml setupString
	H.body $ do
		H.div H.! A.id "container" $ do (sheetHtml 0 0);
									  	(titleHtml x);

makeSheet :: Sheet -> Html
makeSheet (Sheet t fs s) = docTypeHtml $ do
	H.head $ do
		H.meta H.! A.charset "uft-8"
		H.style $ toHtml setupString
	H.body $ do
		H.div H.! A.id "container" $ do (sheetHtml 0 0);
									  (titleHtml t);
									  (songHtml (newManager fs) s);

songHtml :: Manager -> [[[Sound]]] -> Html
songHtml m@(M y x fs) xsss = do fshtml;
							  	(musicHtml newm xsss);
							where (fshtml, newm) = keySigHtml m xsss

newManager :: String -> Manager
newManager fs = (M initY initX fs)

yInc :: Int -> Int
yInc y = y + sizeOfStanza


musicHtml :: Manager -> [[[Sound]]] -> Html
musicHtml m@(M y x fs) (s:ss) = do (printStanza m s);
									musicHtml (M (yInc y) x fs) ss

printStanza :: Manager -> [[Sound]] -> Html
printStanza m@(M y x fs) (s:ss) = "Nope"

keySigHtml :: Manager -> [[[Sound]]] -> (Html, Manager)
keySigHtml m xsss = Prelude.foldl (makeKeySig) ("",m) xsss

makeKeySig :: (Html, Manager) -> [[Sound]] -> (Html, Manager)
makeKeySig (h, (M y x fs)) anything = (nh, (M ny x fs))
		where ny = y + sizeOfStanza;
			  nh = do h;
			          (printKeySig fs y x);

printKeySig :: String -> Int -> Int -> Html
printKeySig fs x y = case (Prelude.head fs) of
			   			('f') -> flatHtml (tail fs) y x
			  			('s') -> sharpHtml (tail fs) y x

flatHtml :: String -> Int -> Int -> Html
flatHtml fs y x = "Nope"

sharpHtml :: String -> Int -> Int -> Html
sharpHtml fs y x  = "Nope"


main = do
	args <- getArgs;
	contents <- readFile (Prelude.head args);
	printTitleError contents;

	case parse sheet "(stdin)" (process contents) of
		Left e ->  do putStrLn "Error parsing input:";
					  print e;
					  
		Right r -> do printSoundError r;
					  print $ createSheet (findTitle contents) (checkFlatsSharps contents) (createMusic r);
					  L.writeFile "output.html" (R.renderHtml (test "Title goes here, it's long isn't it"));