{-# LANGUAGE BangPatterns #-}

module WTrie (WTrie(..), toList, fromFile, fromList) where

import Data.List (intersperse)


data WTrie = WNode { letter :: Char, final :: Bool, children :: [WTrie] }

instance Show WTrie where
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

singleton :: String -> [WTrie]
singleton (c:[]) = [WNode c True  []            ]
singleton (c:cs) = [WNode c False (singleton cs)]

addToTrie :: String -> [WTrie] -> [WTrie]
addToTrie s [] = singleton s
addToTrie (x:[]) ((t@(WNode l f cs)):ts) = if x == l then t{final = True} : ts
                                                     else t               : addToTrie (x:[]) ts
addToTrie (x:xs) ((t@(WNode l f cs)):ts) = if x == l then t{children = addToTrie xs cs} : ts
                                                     else t                             : addToTrie (x:xs) ts

fromFile :: String -> IO [WTrie]
fromFile f = do rawwords <- readFile f
                return $ fromList . words $ rawwords


fromList :: [String] -> [WTrie]
fromList = foldr addToTrie []

toList :: [WTrie] -> [String]
toList = concatMap singleToList
    where singleToList (WNode l f cs) = map (l:) $ (if f then ("":) else id) $ toList cs
