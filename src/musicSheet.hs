-- musicSheet.hs

import System.Environment
import System.IO
import System.Exit
import Text.ParserCombinators.Parsec

--the grammar, yet to be properly defined
sheet = endBy stanza eol
stanza = sepBy measure (char '|')
measure = many (noneOf "|\n")
--measure = sepBy sounds (char ' ')
eol = 	try (string "\n\r")
	<|> try (string "\r\n")
	<|> string "\n"
	<|> string "\r"
	<?>	"end of stanza"

data Tone = Sharp | Flat | Natural | None deriving (Show)
data Note = A | B | C | D | E | F | G | Rest deriving (Show)


--The data object we hope to use to change to html?
data Sheet = Sheet { title :: String
					,flats :: String
					,sharps :: String
					,song :: [[Sound]]
				   } deriving (Show)

data Sound = Note { tone :: Tone,
					note :: Note,
					duration :: Int,
					octave :: String
				  } | Chord [Sound] deriving(Show)

emptySheet :: Sheet
emptySheet = Sheet "" "" "" []

--the standalone parse function for parse testing in ghci
parseInput :: String -> Either ParseError [[String]]
parseInput input = parse sheet "(unknown)" input

--appends a necessary newline if the end of the input is missing one
--so the parser doesn't freak out
appendNewline :: String -> String
appendNewline xs = if (last xs /= '\n')
					then xs ++ "\n"
					else xs

ind :: Int -> [a] -> Maybe a
ind x y
	| x >= length y = Nothing
	| otherwise 	= Just (y !! x)

words_ :: Maybe String -> [String]
words_ (Just xs) = words xs
words_ Nothing  = []

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

fromJust :: Maybe a -> a
fromJust (Just a) = a

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
checkSound :: [[String]] -> IO ()
checkSound xs = map 

--creates sound object out of a string like "#A2"
makeSound :: String -> Sound
makeSound 

--creates the double array of Sound objects out of parse output
--for html creation functions
createMusic :: [[String]] -> [[Sound]]
createMusic xs 



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
