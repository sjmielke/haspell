module Spell where

import Data.List (sortBy)
import Data.Ord (comparing)

import WTrie

type MEDTable = [(Char, Int)]
type Result = (String, Int)

-- | A MEDTableState consists of a (one column) table for MED calculationy,
-- the current word the MED is calculated for (careful though, the word is reversed!)
-- and the current best guess new guesses would have to be better than.
type MEDTableState = (MEDTable, String, Int)

calcMED :: String -> [WTrie] -> [Result]
calcMED uword ts = take 10
                 $ sortBy (comparing snd)
                 $ map (\(s,i) -> (reverse s, i))
                 $ reduceTries (initialState uword, (maxBound, 0)) ts

initialState :: String -> MEDTableState
initialState s = (zip (' ':s) [0..], "", maxBound)

reduceTries :: (MEDTableState, (Int, Int)) -> [WTrie] -> [Result]
reduceTries (oldState, (worstResult, numberOfResults)) ts = concatMap reduceIndiv ts
    where
        reduceIndiv :: WTrie -> [Result]
        reduceIndiv (WNode l f cs) = let (newState@(currentTable, currentWord, _), res, currentMED) = calcNode oldState l f
                                         -- The best MED that is still possible in future steps is MED - max(0, wordLengthDifference)
                                         bestMEDStillPossible = currentMED - max 0 ((length currentTable - 1) - (length currentWord))
                                     in res ++ (if bestMEDStillPossible < worstResult
                                                || numberOfResults < 10
                                                then reduceTries (newState, (if null res then worstResult else maximum $ map snd res, numberOfResults + length res)) cs
                                                else [])

calcNode :: MEDTableState -> Char -> Bool -> (MEDTableState, [Result], Int)
calcNode (oldTable, currentWord, currentBest) l f = ((newTable, l:currentWord, currentBest), newRes, newMED)
    where
        -- previous column and the value below the current one
        step :: MEDTable -> Int -> MEDTable
        step (_:[]) _ = []
        -- ci/vi: insertion/left, cs/vs: substitution/lower left, cd/vd: deletion/down
        step ((cs,vs) : i@(ci,vi) : xs) vd = let newVal = if ci == l
                                                          then vs
                                                          else 1 + min vs (min vd vi) -- everything costs 1
                                             in  (ci, newVal) : step (i : xs) newVal
        -- step doesn't create a new #-row, so we manually add it here
        newHash = (1+) . snd . head $ oldTable
        newTable = (' ', newHash) : step oldTable newHash
        newMED = snd . last $ newTable
        newRes = if f
                 then [(l:currentWord, newMED)]
                 else []
