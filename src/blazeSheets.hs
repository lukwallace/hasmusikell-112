{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import Text.ParserCombinators.Parsec as P
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8 as R
import qualified Data.ByteString.Lazy as L
import System.Environment
import MusicSheet

--For keeping track of data maybe? Or if we could learn to do w/ monads
--The first int is the Y position, second is X position, and the String
--represents the flats and sharps in the key signature
data Manager = M Int Int String


initX = 0
initY = 0
sizeOfStanza = 130
sizeOfLetter = 12
sizeOfFlat = 50
sizeOfSharp = 50
sizeOfUnit = 5
sizeOfMeasure = 275
halfOfStanza = 700


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
			  "#flat{z-index:100;position:absolute;height: 50px;width: 50px;}"++
			  "#natural{z-index:100;position:absolute;height: 60px;width: 50px;}"++
			  "#sharp{z-index:100;position:absolute;height: 60px;width: 50px;}"++
			  "#commontime{z-index:100;position:absolute;height: 50px;width: 50px;}"++
			  "#beight{z-index:100;position:absolute;height: 50px;width: 50px;}" ++
			  "#line{z-index:100;position:absolute;height: 50px;width: 50px;}"

sheetHtml :: Int -> Int -> Html
sheetHtml a b = H.img ! A.style (toValue(str)) ! A.id "image" ! A.src "img/newSheet.png"
    		 where str = "top:" ++ show a ++ "px; left:" ++ show b ++ "px;"


titleHtml :: String -> Html
titleHtml xs =  H.h1 ! A.style (toValue("top:0px; left:" ++ show a ++ "px;")) ! A.id "title" $ toHtml xs
				where a = halfOfStanza - (0.5*(fromIntegral(length xs))*sizeOfLetter)

unitHtml :: Notes -> Float -> Int -> Int -> Html
unitHtml (N n) f a b
    | (f == 1/4 && n /= 'r')      = H.img ! A.style (toValue(str)) ! A.id "fourth" ! A.src "img/4th-note.png"
    | (f == 1/8 && n /= 'r')      = H.img ! A.style (toValue(str)) ! A.id "eighth" ! A.src "img/8th-note.png"
    | (f == 1/16 && n /= 'r')     = H.img ! A.style (toValue(str)) ! A.id "sixteen" ! A.src "img/16th-note.png"
    | (f == 1 && n /= 'r')        = H.img ! A.style (toValue(str)) ! A.id "whole" ! A.src "img/whole-note.png"
    | (f == 1/2 && n /= 'r')      = H.img ! A.style (toValue(str)) ! A.id "half" ! A.src "img/half-note.png"
    | (f == 1 && n == 'r')        = H.img ! A.style (toValue(str)) ! A.id "wholerest" ! A.src "img/wholerest.png"
    | (f == 1/2 && n == 'r')      = H.img ! A.style (toValue(str)) ! A.id "halfrest" ! A.src "img/halfrest.png"
    | (f == 1/4 && n == 'r')      = H.img ! A.style (toValue(str)) ! A.id "forth-rest" ! A.src "img/4th-rest.png"
    | (f == 1/8 && n == 'r')      = H.img ! A.style (toValue(str)) ! A.id "eightrest" ! A.src "img/eightrest.png"
    where str = "top:" ++ show a ++ "px; left:" ++ show b ++ "px;"

unit2Html :: String -> Int -> Int -> Html
unit2Html x a b 
    | x == "commontime"  = H.img ! A.style (toValue(str)) ! A.id "commontime" ! A.src "img/commontime.png"
    | x == "line"        = H.img ! A.style (toValue(str)) ! A.id "line" ! A.src "img/line.png"
    where str = "top:" ++ show a ++ "px; left:" ++ show b ++ "px;"

flatSharpHtml :: Int -> Int -> Tone -> Html
flatSharpHtml y x a
    | a == Flat        = H.img ! A.style (toValue(str)) ! A.id "flag" ! A.src "img/flag.png"
    | a == Natural     = H.img ! A.style (toValue(str)) ! A.id "natural" ! A.src "img/natural.png"
    | a == Sharp       = H.img ! A.style (toValue(str)) ! A.id "sharp" ! A.src "img/sharp.png"
    | otherwise = []
    where str = "top:" ++ show y ++ "px; left:" ++ show x ++ "px;"

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
		H.meta ! A.charset "uft-8"
		H.style $ toHtml setupString
	H.body $ do
		H.div ! A.id "container" $ do (sheetHtml 0 0);
									  	(titleHtml x);

makeSheet :: Sheet -> Html
makeSheet (Sheet t fs s) = docTypeHtml $ do
	H.head $ do
		H.meta ! A.charset "uft-8"
		H.style $ toHtml setupString
	H.body $ do
		H.div ! A.id "container" $ do (sheetHtml 0 0);
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

xInc :: Int -> Int
xInc x = x + sizeOfMeasure


musicHtml :: Manager -> [[[Sound]]] -> Html
musicHtml m@(M y x fs) [] = []
musicHtml m@(M y x fs) (s:ss) = do printStanza m s
                                   musicHtml (M (yInc y) x fs) ss

printStanza :: Manager -> [[Sound]] -> Html
printStanza m@(M y x fs) [] = []
printStanza m@(M y x fs) (s:ss) = do printMeasure m s 
                                     printStanza (M y (xInc x) fs) ss

printMeasure :: Manager -> [Sound] -> Html
printMeasure m@(M y x fs) [] = []
printMeasure m@(M y x fs) (s:ss) = do printNote m s
                                      printMeasure (M y (xDona x s) fs) ss

printNote :: Manager -> Sound -> Html
printNote (M y x fs) s = case s of 
	                       Note {a,b,c,d} -> do noteHtml y x fs a b c d
	                       Chore a      -> do printNote (M y x fs) a!!0
	                                          printNote (M y x fs) (tail a)
	                       otherwise -> []

noteHtml :: Int -> Int -> String -> Tone -> Notes -> Float -> Int -> Html
noteHtml y x fs a b c d = do unitHtml b c (y + scale(b) + (d*35)) x
                             checkFS (y + scale(b) + (d*35)) x a changeFS(fs b)

changeFS :: String -> Notes -> Tone
changeFS [] (N b) = None
changeFS (fs:fss) (N b) = if fs == b then do getFS(last fss) else changeFS fss (N b)

getFS :: Char -> Tone
getFS fss 
     | fss == 'f' = Flat
     | fss == 's' = Sharp

scale :: Notes -> Int
scale b 
    | b == N 'C' = 0
    | b == N 'A' = -10
    | b == N 'B' = -5
    | b == N 'D' = 5
    | b == N 'E' = 10
    | b == N 'F' = 15
    | b == N 'G' = 20
    | b == N 'r' = 0
    | otherwise = 0

checkFS :: Int -> Int -> Tone -> Tone -> Html
checkFS y x a fs 
      | a == fs = []
      | otherwise = flatSharpHtml y (x-25) a
--increment for donation
xDona :: Int -> Sound -> Int
xDona x s = case s of 
	           Note {a,b,c,d} -> x + floor(c*sizeOfMeasure)
	           Chore a      -> xDona x (head a)


keySigHtml :: Manager -> [[[Sound]]] -> (Html, Manager)
keySigHtml m xsss = foldl (makeKeySig) ("",m) xsss


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
flatHtml [] y x = ""
flatHtml f y x = do unitHtml "flat" (yMapping (Prelude.head f) y) x;
					(flatHtml (tail f) y (x+sizeOfFlat));

sharpHtml :: String -> Int -> Int -> Html
sharpHtml [] y x  = ""
sharpHtml s y x = do unitHtml "sharp" (yMapping (Prelude.head s) y) x;
					(sharpHtml (tail s) y (x+sizeOfSharp));

yMapping :: Char -> Int -> Int
yMapping c initY = case c of
	'C' -> initY
	'D' -> initY + (1*sizeOfUnit)
	'E' -> initY + (2*sizeOfUnit)
	'F' -> initY + (3*sizeOfUnit)
	'G' -> initY + (4*sizeOfUnit)
	'A' -> initY + (5*sizeOfUnit)
	'B' -> initY + (6*sizeOfUnit)

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