-- musicSheet.hs

import System.Environment
import System.IO
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

checkTitle :: String -> IO ()
checkTitle xs
	| length firstline < 2 			= putStrLn msg
	| (head firstline) == "title:" 	= print $ unwords $ tail firstline
	| otherwise						= putStrLn msg
	where firstline = words_ $ ind 0 $ lines xs;
		  msg = "title: <your_title> needs to be your first line"


main = do
	args <- getArgs
	contents <- readFile (head args)
	checkTitle contents
	case parse sheet "(stdin)" (appendNewline contents) of
		Left e -> do putStrLn "Error parsing input:";
					 print e
		Right r -> mapM_ print r
