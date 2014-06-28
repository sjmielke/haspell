-- | Supplies a method for calculating the
-- minimum edit distance (MED) from a word
-- to all words of a 'WTrie'.

module TrieMED (
    Result,
    calcMEDs,
    ) where

import Data.List (sort, sortBy, nub)
import Data.Ord (comparing)
import Data.Maybe (maybeToList)

import WTrie
import Confusion

-- | A MEDTable stores the user user input String
-- and the current column of the MED matrix.
type MEDTable = [(Char, Int)]
type Result = (String, Int)

-- | A MEDTableState consists of a (one column) table for MED calculationy,
-- the current word the MED is calculated for (careful though, the word is reversed!).
type MEDTableState = (MEDTable, String)

-- | Traverses the given 'WTrie' succesively building the MED matrices for
-- each word to the given input word, returning a list of the best matches.
calcMEDs :: Int -- ^ the number of suggestions to return
         -> String
         -> WTrie
         -> [Result]
calcMEDs numberOfSuggestions uword ts = map (\(s,i) -> (reverse s, i))
                                      $ take numberOfSuggestions . sortBy (comparing snd)
--                                      $ (\l -> [(uword ++ " compared against #nodes", length l)])
                                      $ reduceTriesAll (initialState uword) ts
                                      -- Usually we want 10 results better than 8, if more are requested,
                                      -- the betterThan-value will grow to encompass all results.
--                                      $ reduceTriesBetterThan (numberOfSuggestions - 2) (initialState uword) ts
--                                      $ reduceTriesKeepTrack (initialState uword, []) ts
    where
        initialState :: String -> MEDTableState
        initialState s = (zip (' ':s) [0..], "")

reduceTriesAll :: MEDTableState -> WTrie -> [Result]
reduceTriesAll oldState ts = concatMap reduceIndiv ts
    where
        reduceIndiv :: WIndivTrie -> [Result]
        reduceIndiv (WNode l f cs) = let (newState@(currentTable, currentWord), res, currentMED) = calcNode oldState l f
                                     in case res of Nothing ->     reduceTriesAll newState cs
                                                    Just r  -> r : reduceTriesAll newState cs

reduceTriesBetterThan :: Int -> MEDTableState -> WTrie -> [Result]
reduceTriesBetterThan b ini ts = filter ((< b) . snd) $ reduceList ini ts
    where
        reduceList :: MEDTableState -> WTrie -> [Result]
        reduceList oldState = concatMap (reduceIndiv oldState)
        reduceIndiv :: MEDTableState -> WIndivTrie -> [Result]
        reduceIndiv oldState (WNode l f cs) = let (newState@(currentTable, currentWord), res, currentMED) = calcNode oldState l f
                                                  -- The best MED that is still possible in future steps is MED - max(0, wordLengthDifference)
                                                  bestMEDStillPossible = currentMED - max 0 ((length currentTable - 1) - (length currentWord))
                                                  followers = if bestMEDStillPossible < b
                                                              then reduceTriesBetterThan b newState cs
                                                              else []
                                              in case res of Nothing ->     followers
                                                             Just r  -> r : followers

calcNode :: MEDTableState -> Char -> Bool -> (MEDTableState, Maybe Result, Int)
calcNode (oldTable, currentWord) l f = ((newTable, l:currentWord), newRes, newMED)
    where
        -- previous column and the value below the current one
        step :: MEDTable -> Int -> MEDTable
        step (_:[]) _ = []
        -- ci/vi: insertion/left/del[], cs/vs: substitution/lower left/sub[], cd==l/vd: deletion/down/add[]
        step ((cs,vs) : i@(ci,vi) : xs) vd = let newVal = if ci == l
                                                          then vs
                                                          else min (vs + subCost)
                                                                   (min (vd + addCost)
                                                                        (vi + delCost))
                                             in  (ci, newVal) : step (i : xs) newVal
            where
                subCost = if cs == l
                          && not (null currentWord)
                          && head currentWord == ci
                          then getRevConfMat ci cs
                          else getSubConfMat ci l
                delCost = getDelConfMat ci l
                addCost = getAddConfMat l ci
        -- step doesn't create a new #-row, so we manually add it here
        newHash = (1+) . snd . head $ oldTable
        newTable = (' ', newHash) : step oldTable newHash
        newMED = snd . last $ newTable
        newRes = if f
                 then Just (l:currentWord, newMED)
                 else Nothing
