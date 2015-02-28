{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8 as R
import qualified Data.ByteString.Lazy as L
import System.Environment
import Text.ParserCombinators.Parsec as P
import Data.List.Split as Z
import Text.Regex.Posix
import MusicSheet


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
			  "#beight{z-index:100;position:absolute;height: 50px;width: 50px;}"

sheets :: Int -> Int -> Html
sheets a b = H.img ! A.style (toValue("top:" ++ show a ++ "px; left:" ++ show b ++ "px;")) ! A.id "image" ! A.src "img/newSheet.png"

test :: String -> Html
test x = docTypeHtml $ do
	H.head $ do
		H.meta ! A.charset "uft-8"
		H.style $ toHtml setupString
	H.body $ do
		H.div ! A.id "container" $ sheets 0 0


titleHtml :: String -> Html
titleHtml xs =  H.h1 ! A.style (toValue("top:0px; left:" ++ show a ++ "px;")) ! A.id "title" $ toHtml xs
				where a = 1500 - (0.5*(fromIntegral(length xs))*10)

noteHtml :: String -> Int -> Int -> Html
noteHtml x a b
    | x == "fourth"      = H.img ! A.style (toValue("top:" ++ show a ++ "px; left:" ++ show b ++ "px;")) ! A.id "fourth" ! A.src "img/4th-note.png"
    | x == "eighth"      = H.img ! A.style (toValue("top:" ++ show a ++ "px; left:" ++ show b ++ "px;")) ! A.id "eighth" ! A.src "img/8th-note.png"
    | x == "sixteen"     = H.img ! A.style (toValue("top:" ++ show a ++ "px; left:" ++ show b ++ "px;")) ! A.id "sixteen" ! A.src "img/16th-note.png"
    | x == "whole"       = H.img ! A.style (toValue("top:" ++ show a ++ "px; left:" ++ show b ++ "px;")) ! A.id "whole" ! A.src "img/whole-note.png"
    | x == "half"        = H.img ! A.style (toValue("top:" ++ show a ++ "px; left:" ++ show b ++ "px;")) ! A.id "half" ! A.src "img/half-note.png"
    | x == "wholerest"   = H.img ! A.style (toValue("top:" ++ show a ++ "px; left:" ++ show b ++ "px;")) ! A.id "wholerest" ! A.src "img/wholerest.png"
    | x == "halfrest"    = H.img ! A.style (toValue("top:" ++ show a ++ "px; left:" ++ show b ++ "px;")) ! A.id "halfrest" ! A.src "img/halfrest.png"
    | x == "forth-rest"  = H.img ! A.style (toValue("top:" ++ show a ++ "px; left:" ++ show b ++ "px;")) ! A.id "forth-rest" ! A.src "img/4th-rest.png"
    | x == "eightrest"   = H.img ! A.style (toValue("top:" ++ show a ++ "px; left:" ++ show b ++ "px;")) ! A.id "eightrest" ! A.src "img/eightrest.png"
    | x == "flag"        = H.img ! A.style (toValue("top:" ++ show a ++ "px; left:" ++ show b ++ "px;")) ! A.id "flag" ! A.src "img/flag.png"
    | x == "natural"     = H.img ! A.style (toValue("top:" ++ show a ++ "px; left:" ++ show b ++ "px;")) ! A.id "natural" ! A.src "img/natural.png"
    | x == "sharp"       = H.img ! A.style (toValue("top:" ++ show a ++ "px; left:" ++ show b ++ "px;")) ! A.id "sharp" ! A.src "img/sharp.png"
    | x == "commontime"  = H.img ! A.style (toValue("top:" ++ show a ++ "px; left:" ++ show b ++ "px;")) ! A.id "commontime" ! A.src "img/commontime.png"

main = do
	args <- getArgs;
	contents <- readFile (Prelude.head args);
	printTitleError contents;

	case parse sheet "(stdin)" (process contents) of
		Left e ->  do putStrLn "Error parsing input:";
					  print e;
					  
		Right r -> do printSoundError r;
					  print $ createSheet (findTitle contents) (checkFlatsSharps contents) (createMusic r);
					  L.writeFile "blaze-test.html" (R.renderHtml (test "Title!"));				  

	     
