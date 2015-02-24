{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8 as R
import qualified Data.ByteString.Lazy as L
import System.Environment


setupString :: String
setupString = "#container{height:2300px;width:3000px;position:relative;}" ++
			  "#image{height:50%;width:50%;position:absolute;}"

sheet :: Html
sheet = H.img ! A.style "top:0px; left:0px;" ! A.id "image" ! A.src "newSheet.png"

test :: String -> Html
test x = docTypeHtml $ do
	H.head $ do
		H.meta ! A.charset "uft-8"
		H.style $ toHtml setupString
	H.body $ do
		H.div ! A.id "container" $ sheet


main = L.writeFile "blaze-test.html" (R.renderHtml (test "Title!"))