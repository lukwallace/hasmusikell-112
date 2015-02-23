-- musicSheet.hs

import System.Environment
import System.IO
import System.Exit
import Text.ParserCombinators.Parsec as P
import Data.List.Split as Z
import Text.Regex.Posix

--the grammar, yet to be properly defined
sheet = P.endBy stanza eol
stanza = P.sepBy measure (char '|')
measure = many (noneOf "|\n")
--measure = sepBy sounds (char ' ')
eol = 	try (string "\n\r")
	<|> try (string "\r\n")
	<|> string "\n"
	<|> string "\r"
	<?>	"end of stanza"

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

--appends a necessary newline if the end of the input is missing one
--so the parser doesn't freak out
appendNewline :: String -> String
appendNewline xs = if (last xs /= '\n')
					then xs ++ "\n"
					else xs

{-======= Utility Functions =======-}

ind :: Int -> [a] -> Maybe a
ind x y
	| x >= length y = Nothing
	| otherwise 	= Just (y !! x)

words_ :: Maybe String -> [String]
words_ (Just xs) = words xs
words_ Nothing  = []

fromJust :: Maybe a -> a
fromJust (Just a) = a

{-===== Error Checking Functions =====-}

checkTitle :: String -> Maybe String
checkTitle xs
	| length firstline < 2 			= Nothing
	| (head firstline) /= "title:" 	= Nothing
	| otherwise						= Just $ unwords $ tail firstline
	where firstline = words_ $ ind 0 $ lines xs;

checkFlatsSharps :: String -> Maybe String
checkFlatsSharps xs
	| length line < 2 			= Nothing
	| (head line) == "flats:" 	= Just $ (lines xs)!!1
	| (head line) == "sharps:" 	= Just $ (lines xs)!!1
	| otherwise				    = Nothing
	where line = words_ $ ind 1 $ lines xs;

findTitle :: String -> String
findTitle xs = title
	where (Just title) = checkTitle xs



process :: String -> String
process xs = if(top == "title:" || top == "flats:" || top == "sharps:")
			 then process $ unlines $ tail $ lines $ xs
			 else appendNewline $ xs
			where top = head $ words $ head $ lines xs


--takes the title, and a maybe string for the flat/sharps and returns
--sheet object with the appropriate fields
addSheetHeader :: String -> Maybe String -> Sheet
addSheetHeader title Nothing   = Sheet title "" "" []
addSheetHeader title (Just line)
	| (head chunks) == "flats:"  = Sheet title (unwords $ tail $ chunks) "" []
	| (head chunks) == "sharps:" = Sheet title "" (unwords $ tail $ chunks) []
	where chunks = words line

--intended input: the result of the parse, should check if the
--array of notes we have are valid notes using regex.
--prints a message if notes are okay, exits program if not

checkSound :: [[String]] -> Bool
checkSound xss = foldl (\acc xs -> acc && (checkInnerSound xs) ) True xss

checkInnerSound :: [String] -> Bool
checkInnerSound xs = foldl (\acc x -> acc && (isNote x || isChord x) ) True xs

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

--Steven's
--checks which note is inputted
{- makeNote :: String -> Notes
makeNote [] = Empty
makeNote xs 
	| (head xs) == "A"	 = A
	| (head xs) == "B"	 = B
	| (head xs) == "C" 	 = C
	| (head xs) == "D" 	 = D
	| (head xs) == "E" 	 = E
	| (head xs) == "F"	 = F
	| (head xs) == "G"	 = G
	| (head xs) == "r" 	 = Rest
	| otherwise			 = makeNote (tail xs)

--from 1-4, each note takes the according number of beats, 
--8 and 16 talk 1/2 and 1/4 respectively
noteDuration :: String -> Int
noteDuration [] = None
noteDuration xs
	| (head xs) == "1" 	 = 1 
	| (head xs) == "2" 	 = 2
	| (head xs) == "3" 	 = 3
	| (head xs) == "4" 	 = 4
	| (head xs) == "5" 	 = 8
	| (head xs) == "6"   = 16
	| otherwise 		 = noteDuration (tail xs)

--checks for tone
checkTone :: String -> Tone 
checkTone [] = Natural
checkTone xs
	| (head xs) == "b" 	 = Flat
	| (head xs) == "#"   = Sharp
	| otherwise			 = checkTone (tail xs)
-}
--Tommy's
--creates sound object out of a string like "#A2"
makeSound :: String -> Sound
makeSound xs 
    |(checkChord xs) >= 1   = returnChord xs
    |otherwise              = returnNote xs

checkChord :: String -> Int
checkChord [] = 0
checkChord xs = noteN (charToString(head xs)) + checkChord (tail xs)

noteN :: String -> Int
noteN xs 
    |xs ==  ","             = 1
    |otherwise              = 0

charToString :: Char -> String
charToString c = [c]

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
    | (head xs) == 'b'      = Flat
    | (head xs) == '#'      = Sharp
    | (head xs) == 'n'      = Natural
    | otherwise             = toneSearch (tail xs)

noteSearch :: String -> Notes
noteSearch [] = Empty
noteSearch xs 
    | (head xs) == 'A'      = A
    | (head xs) == 'B'      = B
    | (head xs) == 'C'      = C
    | (head xs) == 'D'      = D
    | (head xs) == 'E'      = E
    | (head xs) == 'F'      = F
    | (head xs) == 'G'      = G
    | (head xs) == 'r'      = Rest
    | otherwise             = noteSearch (tail xs)

durationSearch :: String -> Float
durationSearch [] = 1
durationSearch xs 
    | (head xs) == '1'      = 1       --whole
    | (head xs) == '2'      = 1/2     --half
    | (head xs) == '3'      = 3/4     --3/4
    | (head xs) == '4'      = 1/4     --4th
    | (head xs) == '5'      = 1/8     --8th
    | (head xs) == '6'      = 1/16    --16th
    | otherwise             = durationSearch (tail xs)

octaveSearch :: String -> Int
octaveSearch [] = 0
octaveSearch xs = octaveS (charToString(head xs)) + octaveSearch (tail xs)

octaveS :: String -> Int
octaveS xs
    | xs == "_"             = -1
    | xs == "'"             = 1
    | otherwise             = 0


--creates the double array of Sound objects out of parse output
--for html creation functions
--createMusic :: [[String]] -> [[[Sound]]]
--createMusic xs 

main = do
	args <- getArgs
	contents <- readFile (head args)
	if((checkTitle contents)==Nothing)
		then do putStrLn "\"title: <your_title>\" needs to be at the top.";
				exitFailure
		else putStrLn ". . . ok title";

	print $ addSheetHeader (findTitle contents) (checkFlatsSharps contents)
	case parse sheet "(stdin)" (process contents) of
		Left e -> do putStrLn "Error parsing input:";
					 print e
		Right r -> mapM_ print r
