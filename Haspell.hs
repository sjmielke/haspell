module Haspell where

import Data.Default (def)
import Data.Char (isLetter)
import Data.List.Split (split, whenElt, dropBlanks, keepDelimsR, onSublist)

import WTrie

-- | Wrap text with given ANSI code and reset code to change display properties.
-- (see @http://en.wikipedia.org/wiki/ANSI_escape_code@ )
ansiWrap :: Int -> String -> String
ansiWrap i = (("\ESC[" ++ (show i) ++ "m")++) . (++"\ESC[0m")

-- | Insert ANSI codes to display the argument with inverted colors.
invertText :: String -> String
invertText = ansiWrap 7

-- | Insert ANSI codes to display the argument in fainter font.
faintText :: String -> String
faintText = ansiWrap 2

data SentencePart = Word String | Punctuation String deriving (Show)
type Sentence = [SentencePart]

-- | Callable entry point for interactive text correction.
correctText :: String -> WTrie -> IO String
correctText txt ts = do newSentences <- correctSentences (segmentText txt) ts
                        return $ renderSentences newSentences

-- | Primitive segmentation of a given user input into 'Sentence's
-- containing 'SentencePart's ('Word's and 'Punctionation')
segmentText :: String -> [Sentence]
segmentText = map segmentSentence . splitOnSubSeqs [". ", "! ", "? "] . (:[])
    where
        splitOnSubSeqs :: [String] -> [String] -> [String]
        splitOnSubSeqs [] text = text
        splitOnSubSeqs (sq:sqs) text = concat
                                     $ map (split (keepDelimsR $ dropBlanks $ onSublist sq))
                                     $ splitOnSubSeqs sqs text
        segmentSentence :: String -> Sentence
        segmentSentence = map (\s@(x:xs) -> if isLetter x then Word s else Punctuation s)
                        . split (dropBlanks $ whenElt (not . isLetter))

-- | Interactively correct a segmented text using a given 'WTrie'.
correctSentences :: [Sentence] -> WTrie -> IO [Sentence]
correctSentences ss ts = return ss

-- | Undo 'segmentText' and restore a 'Sentence' list into plain text.
renderSentences :: [Sentence] -> String
renderSentences = foldr (\s acc -> concatMap renderSentencePart s ++ acc) ""
    where renderSentencePart p = case p of Word s -> s
                                           Punctuation s -> s

{- -- Old inflexible approach to separating words.
type UserText = (String, String, String)

correctText :: WTrie -> String -> IO String
correctText ts txt = do (p, w, n) <- walkWords $ splitFirstWord ("", "", txt)
                        return $ p ++ w ++ n
    where
        walkWords :: UserText -> IO UserText
        walkWords u = do nu@(p, w, n) <- processWord u
                         if null w then return nu else walkWords nu
        processWord :: UserText -> IO (UserText)
        processWord t@(prevText, userWord, nextText) = do let (nPrev, nWord, nNext) = splitFirstWord ("", "", nextText)
                                                          -- putStrLn . show $ t
                                                          putStrLn $ faintText (takeFromEnd 10 prevText) ++ userWord ++ faintText (take 10 nextText)
                                                          correctedWord <- return userWord
                                                          return (prevText ++ correctedWord ++ nPrev, nWord, nNext)
        splitFirstWord :: UserText -> UserText
        splitFirstWord t@(_, _, []) = t
        splitFirstWord (p, w, (n:ns)) = if isLetter n
                                        then splitFirstWord (p, w ++ [n], ns)
                                        else if null w
                                             then splitFirstWord (p ++ [n], w, ns)
                                             else (p, w, (n:ns))
        takeFromEnd i = reverse . take i . reverse
-}

{- -- Later maybe.
import Graphics.Vty
main = do vty <- mkVty def
          let line0 = string (def `withForeColor` green) "Foo"
              line1 = string (def `withForeColor` yellow) "Bar"
              img = line0 <|> line1
              pic = picForImage img
          update vty pic
          e <- nextEvent vty
          shutdown vty
          print $ "Last event was: " ++ show (e :: Event)
-}
