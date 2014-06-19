-- | A simple trie implementation for word lists.

module WTrie (
    -- * Data type
    WIndivTrie(..),
    WTrie,
    
    -- * Generation from and to lists
    toList,
    fromList,
    fromFile,
    
    -- * Helper functions
    contains
    
    ) where

import Data.List (intersperse)

-- | This Trie implementation uses one @WNode@ per @Char@.
-- Such a node can have many following characters and be the end of a word or not.
data WIndivTrie = WNode { letter :: Char, final :: Bool, children :: [WIndivTrie] }
-- | Since a @WIndivTrie@ starts with the first letter, a list is needed for a word list representation.
type WTrie = [WIndivTrie]

instance Show WIndivTrie where
    show = walk 1
        where
            walk indent (WNode l f cs) = (if f then "+-([" ++ [l] ++ "]) "
                                               else "+--(" ++ [l] ++ ")--" )
                                         ++ ( concat
                                            . intersperse "\n"
                                            . applyTail ( ((++) $ concat $ replicate indent "|       ")
                                                        . ("\\"++) . tail)
                                            . map (walk $ indent + 1)
                                            $ cs)
            applyTail :: (a -> a) -> [a] -> [a]
            applyTail _ [] = []
            applyTail f xs = head xs : map f (tail xs)
    showList ts = (++) $ concat . intersperse "\n" $ map show ts

-- | Creates a (simply linear) @WTrie@ from one word.
singleton :: String -> WTrie
singleton (c:[]) = [WNode c True  []            ]
singleton (c:cs) = [WNode c False (singleton cs)]

-- | Adds a word to an existing @WTrie@.
addToTrie :: String -> WTrie -> WTrie
addToTrie s [] = singleton s
addToTrie (x:[]) ((t@(WNode l f cs)):ts) = if x == l
                                           then t{final = True} : ts
                                           else t               : addToTrie (x:[]) ts
addToTrie (x:xs) ((t@(WNode l f cs)):ts) = if x == l
                                           then t{children = addToTrie xs cs} : ts
                                           else t                             : addToTrie (x:xs) ts

-- | Loads a whitespace-separated word list and creates a @WTrie@ out of all words.
fromFile :: String -> IO WTrie
fromFile f = do rawwords <- readFile f
                return $ fromList . words $ rawwords

-- | Creates a @WTrie@ from a word list.
fromList :: [String] -> WTrie
fromList = foldr addToTrie []

-- | Recreates a list of all words contained in a @WTrie@.
toList :: WTrie -> [String]
toList = concatMap singleToList
    where singleToList (WNode l f cs) = map (l:) $ (if f then ("":) else id) $ toList cs

-- | Checks whether a @WTrie@ contains a word.
contains :: WTrie -> String -> Bool
contains _ "" = True
contains [] _ = False
contains ((t@(WNode l f cs)):ts) s@(x:xs) | l == x    = if null xs
                                                        then f
                                                        else contains cs xs
                                          | otherwise = contains ts s
