-- | A simple trie implementation for word lists.

module WTrie (
    -- * Data type
    WIndivTrie(..),
    WTrie,
    
    -- * Generation from and into different representations
    fromList',
    fromList,
    toList,
    fromWLFile,
    toWLFile,
    fromWTFile,
    toWTFile,
    
    -- * Helper functions
    contains
    
    ) where

import Data.List (intersperse, groupBy, sort)

-- | This Trie implementation uses one 'WNode' per 'Char'.
-- Such a node can have many following characters and be the end of a word or not.
data WIndivTrie = WNode { letter :: Char, final :: Bool, children :: [WIndivTrie] } deriving (Show, Read)
-- | Since a 'WIndivTrie' starts with the first letter, a list is needed for a word list representation.
type WTrie = [WIndivTrie]

-- | Simple prettyprinting function for 'WTrie's.
prettyprint :: WTrie -> String
prettyprint ts = concat . intersperse "\n" $ map (ppIndivWalk 1) ts
    where
        ppIndivWalk indent (WNode l f cs) = (if f then "+-([" ++ [l] ++ "]) "
                                                  else "+--(" ++ [l] ++ ")--" )
                                            ++ ( concat
                                               . intersperse "\n"
                                               . applyToTail ( ((++) $ concat $ replicate indent "|       ")
                                                             . ("\\"++) . tail)
                                               . map (ppIndivWalk $ indent + 1)
                                               $ cs)
        applyToTail :: (a -> a) -> [a] -> [a]
        applyToTail _ [] = []
        applyToTail f xs = head xs : map f (tail xs)

-- | Creates a (simply linear) 'WTrie' from one word.
singleton :: String -> WTrie
singleton [] = []
singleton (c:[]) = [WNode c True  []            ]
singleton (c:cs) = [WNode c False (singleton cs)]

-- | Adds a word to an existing 'WTrie'.
addToTrie :: String -> WTrie -> WTrie
addToTrie s [] = singleton s
addToTrie (x:[]) ((t@(WNode l f cs)):ts) = seq ts $
                                           if x == l
                                           then t{final = True} : ts
                                           else t               : addToTrie (x:[]) ts
addToTrie (x:xs) ((t@(WNode l f cs)):ts) = seq ts $
                                           if x == l
                                           then t{children = addToTrie xs cs} : ts
                                           else t                             : addToTrie (x:xs) ts

-- | Loads a whitespace-separated word list and creates a 'WTrie' out of all words.
fromWLFile :: FilePath -> IO WTrie
fromWLFile f = do rawwords <- readFile f
                  return $ fromList . words $ rawwords

-- | Write the word list contained in a  'WTrie' into a file.
toWLFile :: FilePath -> WTrie -> IO ()
toWLFile f ts = writeFile f $ concat . intersperse "\n" $ toList ts

-- | Loads a serialization of a 'WTrie' using the read instance.
fromWTFile :: FilePath -> IO WTrie
fromWTFile f = do rawtrie <- readFile f
                  return $ read rawtrie

-- | Write a 'WTrie' into a file using the show-serialization.
toWTFile :: FilePath -> WTrie -> IO ()
toWTFile f ts = writeFile f (show ts)

-- | Naive approach to create a 'WTrie' from a word list (inserting word by word).
-- Requires megabytes of stack space, but is slightly faster
-- and consumes a little less memory than 'fromList'.
--
-- This version is /NOT/ used in the other methods.
fromList' :: [String] -> WTrie
fromList' = foldr addToTrie []

-- | Idea of creating a more clever 'WTrie' construction from a word list.
-- While it is slightly more expensive in memory and time, it doesn't require
-- megabytes of stack space (in contrast to the naive 'fromList''
-- If one could assume word lists to be sorted, it would be faster,
-- but at least the @Data.List.sort@ call brings it down to the level
-- of the primitive 'fromList''.
--
-- This is the version used in the other methods. 
fromList :: [String] -> WTrie
fromList wl = go $ sort wl
    where go ws = map (nodeFromWordlistTuple . tupleFromWordGroup) wordGroups
            where
                wordGroups = groupBy (\x y -> head x == head y) ws
                tupleFromWordGroup wordGroup = (head $ head wordGroup, map tail wordGroup)
                nodeFromWordlistTuple (c, nws) = let cs  = fromList $ filter (not . null) nws
                                                     fin = any null nws
                                                 in c `seq` WNode c fin cs

-- | Recreates a list of all words contained in a 'WTrie'.
toList :: WTrie -> [String]
toList = concatMap singleToList
    where singleToList (WNode l f cs) = map (l:) $ (if f then ("":) else id) $ toList cs

-- | Checks whether a 'WTrie' contains a word.
contains :: WTrie -> String -> Bool
contains _ "" = True
contains [] _ = False
contains ((t@(WNode l f cs)):ts) s@(x:xs) | l == x    = if null xs
                                                        then f
                                                        else contains cs xs
                                          | otherwise = contains ts s
