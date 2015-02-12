-- musicSheet.hs

import System.Environment
import System.IO

main = do
	args <- getArgs
	contents <- readFile (head args)
	putStr contents

