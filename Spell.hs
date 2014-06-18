module Spell where

import Data.List (sort, sortBy, nub)
import Data.Ord (comparing)
import Data.Maybe (maybeToList)

import WTrie

type MEDTable = [(Char, Int)]
type Result = (String, Int)

-- | A MEDTableState consists of a (one column) table for MED calculationy,
-- the current word the MED is calculated for (careful though, the word is reversed!)
-- and the current best guess new guesses would have to be better than.
type MEDTableState = (MEDTable, String, Int)

calcMED :: String -> [WTrie] -> [Result]
calcMED uword ts = tenBest
                 $ map (\(s,i) -> (reverse s, i))
                 $ reduceTries (initialState uword, []) ts

tenBest :: Ord o => [(a, o)] -> [(a, o)]
tenBest = take 10 . sortBy (comparing snd)

initialState :: String -> MEDTableState
initialState s = (zip (' ':s) [0..], "", maxBound)

reduceTries :: (MEDTableState, [Result]) -> [WTrie] -> [Result]
reduceTries _ [] = []
reduceTries (oldState, resultsSoFar) (t:ts) = let newResList = tenBest $ reduceIndiv t
                                              in newResList ++ reduceTries (oldState, newResList) ts
    where
        reduceIndiv :: WTrie -> [Result]
        reduceIndiv (WNode l f cs) = let (newState@(currentTable, currentWord, _), res, currentMED) = calcNode oldState l f
                                         -- The best MED that is still possible in future steps is MED - max(0, wordLengthDifference)
                                         bestMEDStillPossible = currentMED - max 0 ((length currentTable - 1) - (length currentWord))
                                         newResultList = case res of Nothing -> resultsSoFar
                                                                     Just r  -> tenBest $ updateResList r resultsSoFar
                                         followers = if length newResultList < 10
                                                     || bestMEDStillPossible < maximum (map snd newResultList) -- maximum won't fail. Short-circuit logic, bitches.
                                                     then reduceTries (newState, newResultList) cs
                                                     else []
                                     in case res of Nothing ->     followers
                                                    Just r  -> r : followers
        updateResList r [] = [r]
        updateResList r l@(x:xs) = if snd r <= snd x then r : l else x : updateResList r xs

calcNode :: MEDTableState -> Char -> Bool -> (MEDTableState, Maybe Result, Int)
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
                 then Just (l:currentWord, newMED)
                 else Nothing
