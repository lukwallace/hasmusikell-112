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


initX = 110
initY = 105
sizeOfStanza = 130
sizeOfLetter = 12
sizeOfFlatSharp = 25
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
			  "#natural{z-index:100;position:absolute;height: 50px;width: 50px;}"++
			  "#sharp{z-index:100;position:absolute;height: 50px;width: 50px;}"++
			  "#commontime{z-index:100;position:absolute;height: 50px;width: 50px;}"++
			  "#beight{z-index:100;position:absolute;height: 50px;width: 50px;}" ++
			  "#line{z-index:100;position:absolute;height: 50px;width: 50px;}"

sheetHtml :: Int -> Int -> Html
sheetHtml a b = H.img ! A.style (toValue(str)) ! A.id "image" ! A.src "img/newSheet.png"
    		 where str = "top:" ++ show a ++ "px; left:" ++ show b ++ "px;"


titleHtml :: String -> Html
titleHtml xs =  H.h1 ! A.style (toValue("top:0px; left:" ++ show a ++ "px;")) ! A.id "title" $ toHtml xs
				where a = halfOfStanza - (0.5*(fromIntegral(length xs))*sizeOfLetter)

notesHtml :: Notes -> Float -> Int -> Int -> Html
notesHtml (N n) f a b
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

unitHtml :: String -> Int -> Int -> Html
unitHtml x a b 
    | x == "commontime"  = H.img ! A.style (toValue(str)) ! A.id "commontime" ! A.src "img/commontime.png"
    | x == "line"        = H.img ! A.style (toValue(str)) ! A.id "line" ! A.src "img/line.png"
    where str = "top:" ++ show a ++ "px; left:" ++ show b ++ "px;"

flatSharpHtml :: Int -> Int -> Tone -> Html
flatSharpHtml y x a = case a of
                        (Flat) -> H.img ! A.style (toValue(str)) ! A.id "flat" ! A.src "img/flag.png"
                        (Natural) -> H.img ! A.style (toValue(str)) ! A.id "natural" ! A.src "img/natural.png"
                        (Sharp) ->  H.img ! A.style (toValue(str)) ! A.id "sharp" ! A.src "img/sharp.png"
                        otherwise -> ""
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
songHtml m@(M y x fs) xsss = do (keySigHtml m xsss);
							  	(musicHtml (M y (indent fs x) fs) xsss);

newManager :: String -> Manager
newManager fs = (M initY initX fs)

--indents the x position however many pixels to begin printing
--begin printing the notes after all the key signatures are printed
--edge case: fs cannot be size 1, but this generally is't possible.
indent :: String -> Int -> Int
indent "" x = x
indent fs x = x + ((length fs) * sizeOfFlatSharp)

--increments y position to the next stanza (middle C)
yInc :: Int -> Int
yInc y = y + sizeOfStanza

--increments x position to the next measure
xInc :: Int -> Int
xInc x = x + sizeOfMeasure


musicHtml :: Manager -> [[[Sound]]] -> Html
musicHtml m@(M y x fs) [] = ""
musicHtml m@(M y x fs) (s:ss) = do printStanza m s
                                   musicHtml (M (yInc y) x fs) ss

printStanza :: Manager -> [[Sound]] -> Html
printStanza m@(M y x fs) [] = ""
printStanza m@(M y x fs) (s:ss) = do printMeasure m s 
                                     printStanza (M y (xInc x) fs) ss

printMeasure :: Manager -> [Sound] -> Html
printMeasure m@(M y x fs) [] = ""
printMeasure m@(M y x fs) (s:ss) = do printNote m s
                                      printMeasure (M y (xDona x s) fs) ss

printNote :: Manager -> Sound -> Html
printNote m@(M y x fs) s = case s of 
	                         Note {tone =a,note =b,duration =c,octave = d} -> do noteHtml y x fs a b c d
	                         Chord a      -> do printNote m (a!!0)
	                                            if length a >= 2 then printNote m (Chord (tail a)) else ""

noteHtml :: Int -> Int -> String -> Tone -> Notes -> Float -> Int -> Html
noteHtml y x fs a b c d = do notesHtml b c (y + scale(b) + (d*35)) x
                             checkFS (y + scale(b) + (d*35)) x a (changeFS b fs)

changeFS :: Notes -> String ->Tone
changeFS (N b) [] = None
changeFS (N b) fs = if b `elem` (tail fs) then do getFS(Prelude.head fs) else None

getFS :: Char -> Tone
getFS fss 
     | fss == 'f' = Flat
     | fss == 's' = Sharp
--change y position for each scale
scale :: Notes -> Int
scale b = case b of 
            (N 'C') -> 0
            (N 'A') -> -25
            (N 'B') -> -30
            (N 'D') -> -5
            (N 'E') -> -10
            (N 'F') -> -15
            (N 'G') -> -20
            (N 'r') -> 0 --this shouldn't be zero?
--check for redu
checkFS :: Int -> Int -> Tone -> Tone -> Html
checkFS y x a fs 
      | a == fs = ""
      | otherwise = flatSharpHtml y (x-25) a
--increment for donation
xDona :: Int -> Sound -> Int
xDona x s = case s of 
	           Note {tone =a,note =b,duration =c,octave = d}  -> (x+ floor(c * fromIntegral sizeOfMeasure))
	           Chord a      -> xDona x (Prelude.head a)


keySigHtml :: Manager -> [[[Sound]]] -> Html
keySigHtml m xsss = newhtml
		where (newhtml, _) = foldl (makeKeySig) ("",m) xsss

makeKeySig :: (Html, Manager) -> [[Sound]] -> (Html, Manager)
makeKeySig (h, (M y x fs)) anything = (nh, (M ny x fs))
		where ny = y + sizeOfStanza;
			  nh = do h;
			          (printKeySig fs y x);

printKeySig :: String -> Int -> Int -> Html
printKeySig fs y x = case (Prelude.head fs) of
			   			('f') -> flatHtml (tail fs) y x
			  			('s') -> sharpHtml (tail fs) y x

flatHtml :: String -> Int -> Int -> Html
flatHtml [] y x = ""
flatHtml f y x = do flatSharpHtml (yMapping (Prelude.head f) y) x Flat;
					(flatHtml (tail f) y (x+sizeOfFlatSharp));

sharpHtml :: String -> Int -> Int -> Html
sharpHtml [] y x  = ""
sharpHtml s y x = do flatSharpHtml (yMapping (Prelude.head s) y) x Sharp ;
					 (sharpHtml (tail s) y (x+sizeOfFlatSharp));

yMapping :: Char -> Int -> Int
yMapping c initY = case c of
	'F' -> initY - (3*sizeOfUnit)
	'G' -> initY - (4*sizeOfUnit)
	'A' -> initY - (5*sizeOfUnit)
	'B' -> initY - (6*sizeOfUnit)
	'C' -> initY - (7*sizeOfUnit)
	'D' -> initY - (8*sizeOfUnit)
	'E' -> initY - (9*sizeOfUnit)

main = do
	args <- getArgs;
	contents <- readFile (Prelude.head args);
	printTitleError contents;

	case parse sheet "(stdin)" (process contents) of
		Left e ->  do putStrLn "Error parsing input:";
					  print e;
					  
		Right r -> do printSoundError r;
					  print $ createSheet (findTitle contents) (checkFlatsSharps contents) (createMusic r);
					  L.writeFile "output.html" (R.renderHtml (makeSheet te));
					   where te = createSheet (findTitle contents) (checkFlatsSharps contents) (createMusic r);