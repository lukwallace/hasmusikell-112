Project Name : Hasmusikell
Author: Wallace Luk
        Tommy Li
        Steven Cruz
        
Feature Specifications


* Requires a title of song in order for the program to run (will throw out error warnings)
* Optional uses of flats and sharps
* Tempo is default as common time
* All notes must equate to 4 beats per measure unless otherwise mentioned
* Sheet music always starts at middle C unless specified




Configuration Characteristics 


* Each note has a specific duration in terms of beats
* If sharps and flats are not defaulted, it can be added within the stanzas, and as well as naturals
* Notes can be played octaves up or down
* Filters out the last stanza of the sheet music to however played out


Definitions 
* BPM (beats per measure)
* Measures consists of 4 beats
* Durations for each note are followed as:
1. 4 is a quarter note (1BPM), 3 is dotted half (3BPM), 2 is half (2BPM), 1 is full (4BPM)
2. C’ (represents C an octave up), C_ (octave down)
3. | separates each measure
4. \n splits to new stanza
5. nC is natural C, bC is flat C, and #C is sharp
6. [(C,D,E]1) is a full chord note


Basic Example (Numbers represented by line numbers)
1. title: Yo-ho-ho 
2. flats: BCD
3. C4 C4 C4 C4 | bD1 | nE2_ F2’ | D3 C4_ 
4. r2 r2| C1| #B2 E2,G2| F4 F4 F4 F4

Compile Instruction:
1.Save your music input into a .txt file.
2.Move your .txt file to the /src directory with blazeSheets.hs
3.Open command prompt and run "runhaskell blazeSheet.hs blah.txt" in the /src directory
4.Now output.html was created in the same directory, and that's your sheet music
