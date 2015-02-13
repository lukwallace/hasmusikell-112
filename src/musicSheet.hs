-- musicSheet.hs

import System.Environment
import System.IO
import Text.ParserCombinators.Parsec

sheet = endBy stanza eol
stanza = sepBy measure (char '|')
measure = many (noneOf "|\n")
eol = char '\n'

parseInput :: String -> Either ParseError [[String]]
parseInput input = parse sheet "(unknown)" input

main = do
	args <- getArgs
	contents <- readFile (head args)
	mapM putStrLn (parseInput contents)

