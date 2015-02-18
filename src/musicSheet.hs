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
	| (head line) == "flats:" 	= Just "flats"
	| (head line) == "sharps:" 	= Just "sharps"
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
main = do
	args <- getArgs
	contents <- readFile (head args)
	if((checkTitle contents)==Nothing)
		then do putStrLn "\"title: <your_title>\" needs to be at the top.";
				exitFailure
		else putStrLn ". . . ok title";

	if((checkFlatsSharps contents) == Nothing)
	    then do putStrLn "no flats nor sharps";
	    else if (fromJust (checkFlatsSharps contents) == "flats")
	    	then do putStrLn "has flats";
	    	else putStrLn "has sharps";

	print $ findTitle contents
	case parse sheet "(stdin)" (process contents) of
		Left e -> do putStrLn "Error parsing input:";
					 print e
		Right r -> mapM_ print r
