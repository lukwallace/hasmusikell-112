{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8 as R
import qualified Data.ByteString.Lazy as B
import System.Environment

numbers :: Int -> Html
numbers n = docTypeHtml $ do
	H.head $ do
		H.title "Natural numbers"
	body $ do
		p "A list of natural numbers:"
		ul $ forM_ [1 .. n] (li . toHtml)

main = B.writeFile "blaze-test.html" (R.renderHtml (numbers 5))