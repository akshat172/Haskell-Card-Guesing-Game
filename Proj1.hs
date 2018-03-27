--  File        : Proj1.hs
--  Size        : 10 KB
--  Student id  : 893717
--  Author      : Akshat Gupta
--  Purpose     : Project 1 (Declarative Programming)
--  Modified    : 2017-08-31 22:16:15

------------------------------------------------------------------------------

                               --Introduction

------------------------------------------------------------------------------

{-
  The following project is a 2 player musical game. player 1 is a composer who
  ill give a set of pitches known as a chord. A chord contains three pitches. 
  Each pitch is a combination of a note and an octave.A typical note lies in 
  the set [A,G] and an Octave in [1,3]. 
  Player 2's task is to guess the chord in the least possible guesses.
  This game is easy to play but the algorithm behind can be very computational
  demanding for the best average of the guesses.

  KeyWords:
	mch 		: match
	srchSpce 	: searchSpace
	possTrg     : possibleTarget
	prvGss		: previousGuess
	mchNt		: matchNote
-}

------------------------------------------------------------------------------

module Proj1 (initialGuess, nextGuess, GameState) where

import Data.List

------------------------------------------------------------------------------

                            --Setting up the layout

------------------------------------------------------------------------------

{-
The Game State is defined which stores the Last guess along with the response 
feedback score of the mch with the chord and the target. It also contans the 
target space which is initially all possible combinations of chords which will
be filtered based on the response feedback.
-}
data GameState = GameState [([String],Int,Int,Int)] [[String]]
  deriving(Show)

{-
The Compare Type consists of the following three types which will help us in 
distinguishing when we are doing the comparison of the guess with the target 
and finding out the response feedback score to filter out irrelevant chords 
from the search space. 
-}

data SoundType = Pitch | Note | Octave
                    deriving (Show, Eq, Ord)

{-
A pitch comprises of a Note (A-G) and an Octave (1-3). For example "A1","B2" 
etc. the following indexes will help to index the Notes and Octaves 
respectively. Index O for a note and index 1 for an octave.
-}

indexNote :: Int
indexNote = 0 

indexOctave :: Int
indexOctave = 1

------------------------------------------------------------------------------

                             --Guessing the chord

------------------------------------------------------------------------------

{-
The inital guess function takes no input and returns an output which consists 
of an initial guess and the initial game state which is the set of all 
possible chord combinations. The initial Guess is hard coded and can be 
changed on careful analysis so as we get an optimal solution with 4.3 guesses 
approximately on an average.
-}

initialGuess :: ([String], GameState)
initialGuess = (["A1", "D2", "F3"], GameState [] (getAllChords))

------------------------------------------------------------------------------

------------------------------------------------------------------------------

{-
This function will create all the possible combination and return the target 
space back to the initial guess function which will store it in the Game State 
and will be used later for mching with the next subsequent guesses.
-}

getAllChords :: [[String]]
getAllChords =  [ [a,b,c] | a <- combination, b <-combination ,
                 c <- combination , a<b, b<c]
  where octave = [1,2,3]
        notes = ["A","B","C","D","E", "F", "G"]
        combination = [y ++ (show x) | x <- octave, y <- notes]

------------------------------------------------------------------------------

------------------------------------------------------------------------------

{-
The Next Guess function is called recursively to find the perfect guess to the
target. The function takes input from the initial guess function initially and
then updates the game state by the response feedback score and the guess which
will be fed again to the function and further comparison can be done. At every
call of this function the search space which constitutes all the possible 
chords is filtered so that the following space is reduced
-}

nextGuess :: ([String], GameState) -> (Int,Int,Int) -> ([String], GameState)
nextGuess (prvGss,GameState [] srchSpce) (mch,mchNt,mchOct) = 
    (guess', GameState [(prvGss,mch,mchNt,mchOct)] srchSpce')
  where (guess',srchSpce') = possTrg [(prvGss,mch,mchNt,mchOct)] srchSpce
nextGuess (prvGss, GameState (x:xs) srchSpce) (mch, mchNt, mchOct) = 
    (guess', GameState ((prvGss,mch,mchNt,mchOct):x:xs) srchSpce')
  where (guess',srchSpce') = possTrg ((prvGss,mch,mchNt,mchOct):x:xs) srchSpce

------------------------------------------------------------------------------

------------------------------------------------------------------------------

{-
The following function will take the previous guess records and its response
feedback score alongwith the chord space as an input. It will then check the
guess records and the chordspace for the next possible guess and return the
filtered chordspace along with the new guess. To achieve this the function 
will call isTarget function
-}

possTrg :: [([String],Int,Int,Int)] -> [[String]] -> ([String],[[String]])
possTrg guessRecords (possTarget:srchSpce) =
  if isTarget possTarget guessRecords then (possTarget, srchSpce)
  else possTrg guessRecords srchSpce

------------------------------------------------------------------------------

------------------------------------------------------------------------------
{-
The isTarget function will be called from the possTrgFunction with the
first chord from the chord space along with the previous guess records and
will return a boolean value depending on the score from the input and
calculating the new score based on the new score. If the function will return
true then the corresponding guess and the chord space will act as the new 
chordspace and the next guess.
-}

isTarget :: [String] -> [([String],Int,Int,Int)] -> Bool
isTarget target [] = True
isTarget target ( (guess, mch, mchNt, mchOct):xs ) = 
    isGuessIdentical && isTarget target xs
  where (mchPitch', mchNt', mchOct') = getScoreTuple guess target
        isGuessIdentical = 
            mchPitch'==mch && mchNt'==mchNt && mchOct'==mchOct

------------------------------------------------------------------------------

------------------------------------------------------------------------------

{-
The getScoreTuple function will take the current guess and the current target
from the chordspace and calculate the respone feedback score and return it to
the isTarget Function for comparing the results. To achieve this the function
will call the getFeedbackScore function corresponding to the soundtype i.e a 
Pitch, a Note or an Octave.
-}

getScoreTuple :: [String] -> [String] -> (Int,Int,Int)
getScoreTuple guess target = (mch, noteMch, octaveMch)
  where mch =   getFeedbackScore Pitch guess target
        noteMch =   getFeedbackScore Note guess target
        octaveMch =   getFeedbackScore Octave guess target

------------------------------------------------------------------------------

------------------------------------------------------------------------------

{-
--The getFeedbackScore function will do the comparison of the pitches and send
back the score in accordance to the mches i.e if the pitch mches perfectly
or if the notes are mching or the Octaves are mching. This will give the 
corresponding score to each comparison which is the response feedback and will
be used to filter out the chord space and get the target perfectly.The function
takes the sound type, the guess chord and the target chord and compare the two.
-}

getFeedbackScore :: SoundType -> [String] -> [String] -> Int
getFeedbackScore soundType guess target =
  if soundType == Pitch then mchPitch
  else if soundType == Note then noteMch
  else octaveMch
  where mchPitch = length (intersect guess1 target)
        tc = length guess1
        noteMch = tc - noteLength - mchPitch
        octaveMch = tc - octLength - mchPitch
        guess1 = nub guess
        noteLength = length(deleteFirstsBy(getSoundType Note) guess1 target)
        octLength = length(deleteFirstsBy(getSoundType Octave) guess1 target)

------------------------------------------------------------------------------

------------------------------------------------------------------------------

{-
The getSoundType function takes sound type and two lists as input and returns 
a boolean value if the elements mches. The list contains each individual pitch
from the chords and then mch the Note or Octave in accordance to the sound 
type and returns a boolean value which is then used as a predicate for the 
deleteFirstsBy function to get the list with the mching notes and octaves.
-}

getSoundType :: Eq a => SoundType -> [a] -> [a] -> Bool
getSoundType soundType l1 l2 =
  if soundType == Note then (l1 !! indexNote) == (l2 !! indexNote) 
  else (l1 !! indexOctave) == (l2 !! indexOctave)


 ------------------------------END OF FILE------------------------------------