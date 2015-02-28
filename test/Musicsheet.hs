-- musicSheet.hs
{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import System.IO
import System.Exit
import Text.ParserCombinators.Parsec as P
import Data.List.Split as Z
import Text.Regex.Posix
import Control.Monad (forM_)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8 as R
import qualified Data.ByteString.Lazy as L

--the grammar, yet to be properly defined
sheet = P.endBy stanza eol
stanza = P.sepBy measure (char '|')
measure = many (noneOf "|\n")
--measure = sepBy sounds (char ' ')
eol = 	try (P.string "\n\r")
	<|> try (P.string "\r\n")
	<|> P.string "\n"
	<|> P.string "\r"
	<?>	"end of stanza"

{-=================The Data Representations =====================-}

data Tone = Sharp | Flat | Natural | None deriving (Show)
data Notes = A | B | C | D | E | F | G | Rest | Empty deriving (Show)

data Sheet = Sheet { title :: String
					,flats :: String
					,sharps :: String
					,song :: [[[Sound]]]
				   } deriving (Show)

data Sound = Note { tone :: Tone,
					note :: Notes,
					duration :: Float,
					octave :: Int
				  } | Chord [Sound] deriving(Show)



--the standalone parse function for parse testing in ghci
parseInput :: String -> Either ParseError [[String]]
parseInput input = parse sheet "(unknown)" input

{-======= Basic Utility Functions =======-}

ind :: Int -> [a] -> Maybe a
ind x y
	| x >= length y = Nothing
	| otherwise 	= Just (y !! x)

words_ :: Maybe String -> [String]
words_ (Just xs) = words xs
words_ Nothing  = []

fromJust :: Maybe a -> a
fromJust (Just a) = a

charToString :: Char -> String
charToString c = [c]

{-====== Parser Output Utilities ======-}

--strips the title and flat/sharp declarations to create a string
--that's just the stanzas as well as appends a new line so the parser
--doesn't do any freaking out of any sort
process :: String -> String
process xs = if(top == "title:" || top == "flats:" || top == "sharps:")
			 then process $ unlines $ tail $ lines $ xs
			 else appendNewline $ xs
			where top = head $ words $ head $ lines xs

--appends a necessary newline if the end of the input is missing one
--so the parser doesn't freak out
appendNewline :: String -> String
appendNewline xs = if (last xs /= '\n')
					then xs ++ "\n"
					else xs

{-===== Error Checking Functions =====-}

--For the title
printTitleError :: String -> IO ()
printTitleError x = do if((checkTitle x) == Nothing)
						then do putStrLn "\"title: <your_title>\" needs to be at the top.";
							 	exitFailure;
						else putStrLn ". . . ok title";

checkTitle :: String -> Maybe String
checkTitle xs
	| length firstline < 2 			= Nothing
	| (head firstline) /= "title:" 	= Nothing
	| otherwise						= Just $ unwords $ tail firstline
	where firstline = words_ $ ind 0 $ lines xs;

--For the flats and sharps, does not return an IO action because
--it's okay if the user doesn't specify any flats or sharps
checkFlatsSharps :: String -> Maybe String
checkFlatsSharps xs
	| length line < 2 			= Nothing
	| (head line) == "flats:" 	= Just $ (lines xs)!!1
	| (head line) == "sharps:" 	= Just $ (lines xs)!!1
	| otherwise				    = Nothing
	where line = words_ $ ind 1 $ lines xs;

--For the rest of the sheet
printSoundError :: [[String]] -> IO ()
printSoundError xss = let (str, bool) = (checkSound xss) in
					  do if(bool == False) 
						then do putStrLn ("\"" ++ str ++ "\" is not a valid note or chord");
								exitFailure
						else putStrLn ". . . ok sheet";

checkSound :: [[String]] -> (String, Bool)
checkSound listOfStanzas = foldl (findErrorInStanza) ("",True) listOfStanzas

findErrorInStanza :: (String, Bool) -> [String] -> (String, Bool)
findErrorInStanza (accS, accB) listOfMeasures
	| (accB == False) = (accS, accB)
	| (bool == False) = (str, bool)
	| otherwise       = ("", bool)
	where (str, bool) = foldl (findErrorInMeasure) ("", True) listOfMeasures

findErrorInMeasure :: (String, Bool) -> String -> (String, Bool)
findErrorInMeasure (accS, accB) listOfSounds
	| (accB == False) = (accS, accB)
	| (bool == False) = (str, bool)
	| otherwise       = ("", bool)
	where (str, bool) = foldl (validSound) ("", True) (words listOfSounds)

validSound :: (String, Bool) -> String -> (String, Bool)
validSound (accS, accB) x
	| (accB == False) = (accS, accB)
	| (bool == False) = (str, bool)
	| otherwise       = ("", bool)
	where (str, bool) = (x,(isNote x || isChord x))

isNote :: String -> Bool
isNote x =  let notePattern = "([#bn]?[ABCDEFGr][123456]([_']*))" in
			exactMatch $ (x =~ notePattern :: (String,String,String))

isChord :: String -> Bool
isChord xs
	| (length chunks) == 1 = False
	| otherwise            = and $ map (isNote) chunks
	where chunks = Z.splitOn "," xs

exactMatch :: (String, String, String) -> Bool
exactMatch (x, y, z)
	| (x == "") && (y /= "") && (z == "") = True
	| otherwise                           = False


{-====== Parser Output to Data Representation ======-}

--takes the title, and a maybe string for the flat/sharps and returns
--sheet object with the appropriate fields
createSheet :: String -> Maybe String -> [[[Sound]]]-> Sheet
createSheet title Nothing xsss  = Sheet title "" "" xsss
createSheet title (Just line) xsss
	| (Prelude.head chunks) == "flats:"  = Sheet title (unwords $ tail $ chunks) "" xsss
	| (Prelude.head chunks) == "sharps:" = Sheet title "" (unwords $ tail $ chunks) xsss
	where chunks = words line

findTitle :: String -> String
findTitle xs = title
	where (Just title) = checkTitle xs

--creates the triple array of Sound objects out of parse output
--for html creation functions
createMusic :: [[String]] -> [[[Sound]]]
createMusic [] = []
createMusic (x:xs) = [unpage(x)] ++ createMusic(xs)

unpage :: [String] -> [[Sound]]
unpage [] = []
unpage (x:xs) = [unstanza(x)] ++ unpage(xs)

unstanza :: String -> [Sound]
unstanza xs = unmeasure (words xs)

unmeasure :: [String] -> [Sound]
unmeasure [] = []
unmeasure (x:xs) = [makeSound(x)] ++ unmeasure(xs)

makeSound :: String -> Sound
makeSound xs 
    |(checkChord xs) >= 1   = returnChord xs
    |otherwise              = returnNote xs

checkChord :: String -> Int
checkChord [] = 0
checkChord xs = noteN (charToString(Prelude.head xs)) + checkChord (tail xs)

noteN :: String -> Int
noteN xs 
    |xs ==  ","             = 1
    |otherwise              = 0

returnChord :: String -> Sound
returnChord xs = Chord (rChord (Z.splitOn "," xs ))

rChord :: [String] -> [Sound]
rChord [] = []
rChord (x:xs) = [returnNote x] ++ rChord xs

returnNote :: String -> Sound
returnNote xs = Note {tone = toneSearch xs, note = noteSearch xs, duration = durationSearch xs, octave = octaveSearch xs}

toneSearch :: String -> Tone
toneSearch [] = None
toneSearch xs 
    | (Prelude.head xs) == 'b'      = Flat
    | (Prelude.head xs) == '#'      = Sharp
    | (Prelude.head xs) == 'n'      = Natural
    | otherwise             = toneSearch (tail xs)

noteSearch :: String -> Notes
noteSearch [] = Empty
noteSearch xs 
    | (Prelude.head xs) == 'A'      = A
    | (Prelude.head xs) == 'B'      = B
    | (Prelude.head xs) == 'C'      = C
    | (Prelude.head xs) == 'D'      = D
    | (Prelude.head xs) == 'E'      = E
    | (Prelude.head xs) == 'F'      = F
    | (Prelude.head xs) == 'G'      = G
    | (Prelude.head xs) == 'r'      = Rest
    | otherwise             = noteSearch (tail xs)

durationSearch :: String -> Float
durationSearch [] = 1
durationSearch xs 
    | (Prelude.head xs) == '1'      = 1       --whole
    | (Prelude.head xs) == '2'      = 1/2     --half
    | (Prelude.head xs) == '3'      = 3/4     --3/4
    | (Prelude.head xs) == '4'      = 1/4     --4th
    | (Prelude.head xs) == '5'      = 1/8     --8th
    | (Prelude.head xs) == '6'      = 1/16    --16th
    | otherwise             = durationSearch (tail xs)

octaveSearch :: String -> Int
octaveSearch [] = 0
octaveSearch xs = octaveS (charToString(Prelude.head xs)) + octaveSearch (tail xs)

octaveS :: String -> Int
octaveS xs
    | xs == "_"             = -1
    | xs == "'"             = 1
    | otherwise             = 0

{-======Data Representation to Html Object ======-}

setupString :: String
setupString = "#container{height:2300px;width:3000px;position:relative;}" ++
			  "#image{height:50%;width:50%;position:absolute;}"

makeSheet :: Html
makeSheet = H.img ! A.style "top:0px; left:0px;" ! A.id "image" ! A.src "newSheet.png"

test :: String -> Html
test x = docTypeHtml $ do
	H.head $ do
		H.meta ! A.charset "uft-8"
		H.style $ toHtml setupString
	H.body $ do
		H.div ! A.id "container" $ sheet

{-===============================================-}
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
