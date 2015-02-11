-- htmlMusicSheets.hs

import System.Environment
import System.IO

main = do
	file <- getArgs
	contents <- readFile file
	putStr contents

