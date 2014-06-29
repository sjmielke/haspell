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

-- | A MEDTable stores the user user input String
-- and the previous and current column of the MED matrix.
type MEDTable = [(Char, Int, Int)]
type Result = (String, Int)

-- | A MEDTableState consists of a (one column) table for MED calculationy,
-- the current word the MED is calculated for (careful though, the word is reversed!).
type MEDTableState = (MEDTable, String)

-- | Traverses the given 'WTrie' succesively building the MED matrices for
-- each word to the given input word, returning a list of the best matches.
calcMEDs :: Int      -- ^ the number of suggestions to return
         -> String   -- ^ the user word to compare against
         -> WTrie    -- ^ the 'WTrie' containing the word list
         -> [Result] -- ^ the (unsorted) list of corrections
calcMEDs numberOfSuggestions uword ts = map (\(s,i) -> (reverse s, i))
                                      $ take numberOfSuggestions . sortBy (comparing snd)
--                                      $ (\l -> [(uword ++ " compared against #nodes", length l)])
--                                      $ reduceTriesAll (initialState uword) ts
                                      -- Usually we want 10 results better than 8, if more are requested,
                                      -- the betterThan-value will grow to encompass more and more all results.
                                      $ reduceTriesBetterThan (numberOfSuggestions - 2) (initialState uword) ts
    where
        -- The unitinalized column is filled with -1.
        initialState :: String -> MEDTableState
        initialState s = (zip3 (' ':s) (repeat (-1)) [0..], "")

-- | Primitive version that compares a word (implicitly given in a 'MEDTableState')
-- against all nodes of a given 'WTrie' using previous calculations in the 'MEDTableState'
-- to return a list of all possible 'Result's.
reduceTriesAll :: MEDTableState -> WTrie -> [Result]
reduceTriesAll oldState ts = concatMap reduceIndiv ts
    where
        reduceIndiv :: WIndivTrie -> [Result]
        reduceIndiv (WNode l f cs) = let (newState@(_, _), res, _) = calcNode oldState l f
                                     in case res of Nothing ->     reduceTriesAll newState cs
                                                    Just r  -> r : reduceTriesAll newState cs

-- | Compares a word (implicitly given in a 'MEDTableState') against
-- all nodes of a given 'WTrie' that can still yield a 'Result' better than a given threshold
-- using previous calculations in the 'MEDTableState' to return a list of all possible 'Result's.
reduceTriesBetterThan :: Int -> MEDTableState -> WTrie -> [Result]
reduceTriesBetterThan b ini ts = filter ((< b) . snd) $ reduceList ini ts
    where
        reduceList :: MEDTableState -> WTrie -> [Result]
        reduceList oldState = concatMap (reduceIndiv oldState)
        reduceIndiv :: MEDTableState -> WIndivTrie -> [Result]
        reduceIndiv oldState (WNode l f cs) = let (newState@(_, _), res, bestMEDStillPossible) = calcNode oldState l f
                                                  followers = if bestMEDStillPossible < b
                                                              then reduceTriesBetterThan b newState cs
                                                              else []
                                              in case res of Nothing ->     followers
                                                             Just r  -> r : followers

-- | Transforms the previous 'MEDTableState' into a new state at and using the current 'WTrie' node.
calcNode :: MEDTableState -- ^ previous state
         -> Char          -- ^ character at the current 'WTrie' node
         -> Bool          -- ^ final-flag of the current 'WTrie' node
         -> (MEDTableState, Maybe Result, Int) -- ^ calculated new state, a possible found correction and the new minimum MED
calcNode (oldTable, currentWord) l f = ((newTable, l:currentWord), newRes, minimum $ map thrd newTable)
    where
        step :: MEDTable  -- ^ previous column/s
             -> Int       -- ^ the value below the current one (so the one just calculated)
             -> Int       -- ^ the value twice to the lower left (rev base)
             -> MEDTable
        step (_:[]) _ _ = []
        -- ci/vi: insertion/left/del[], 
        -- cs/vs: substitution/lower left/sub[],
        -- cd==l/vd: deletion/down/add[],
        -- vr: reversal/twice lower left / nextRev will be vr in the next step (lower left left)
        step ((cs,nextRev,vs) : i@(ci,_,vi) : xs) vd vr =
                let newVal = trySub . tryRev $ 1 + min vs (min vd vi)
                in  (ci, vi, newVal) : step (i : xs) newVal nextRev
            where
                trySub other = if ci == l
                               then vs
                               else other
                tryRev other = if vr > (-1)
                               && cs == l
                               && not (null currentWord)
                               && head currentWord == ci
                               then min other (1 + vr)
                               else other
        -- step doesn't create a new #-row, so we manually add it here
        newHash = (1+) . thrd . head $ oldTable
        newTable = (' ', newHash - 1, newHash) : step oldTable newHash (-1)
        newRes = if f
                 then Just (l:currentWord, thrd . last $ newTable)
                 else Nothing
        bestMEDStillPossible = let prevMEDs = map scnd newTable
                               in minimum $ (2 + head prevMEDs) : (1 + head (tail prevMEDs)) : (tail $ tail prevMEDs)
        scnd (_,x,_) = x
        thrd (_,_,x) = x
