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
import Musicsheet


setupString :: String
setupString = "#container{height:2300px;width:3000px;position:relative;}" ++
			  "#image{height:50%;width:50%;position:absolute;}"++
			  "#fourth{z-index:100;position:absolute;height: 50px;width: 50px;}"++
			  "#eighth{z-index:100;position:absolute;height: 50px;width: 50px;}"++
			  "#sixteen{z-index:100;position:absolute;height: 50px;width: 50px;}"++
			  "#th32{z-index:100;position:absolute;height: 50px;width: 50px;}"++
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

sheets :: Html
sheets = H.img ! A.style "top:0px; left:0px;" ! A.id "image" ! A.src "newSheet.png"

test :: String -> Html
test x = docTypeHtml $ do
	H.head $ do
		H.meta ! A.charset "uft-8"
		H.style $ toHtml setupString
	H.body $ do
		H.div ! A.id "container" $ sheets



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

	     
