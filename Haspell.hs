module Main where

import Data.Default (def)
import Data.Char (isLetter)
import Data.List.Split (split, whenElt, dropBlanks, keepDelimsR, onSublist)
import Text.Read (readMaybe)
import System.Exit (exitSuccess)

import WTrie
import TrieMED

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
correctText txt ts = do newSentences <- mapM (correctSentence ts) (segmentText txt)
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

-- | Interactively correct a segmented sentence using a given 'WTrie'.
correctSentence :: WTrie -> Sentence -> IO Sentence
correctSentence ts s = mapM correctSentencePart sWithIds
    where
        sWithIds = zip [0..] s
        correctSentencePart :: (Int, SentencePart) -> IO SentencePart
        correctSentencePart (i, p@(Punctuation _)) = return p
        correctSentencePart (i, Word w) = do if ts `contains` w
                                             then return $ Word w
                                             else do
                                                  let results = calcMEDs w ts
                                                      errorCase = do putStrLn $ invertText "What the fuck is wrong with you? Try again."
                                                                     correctSentencePart (i, Word w)
                                                  putStrLn $ highlightInSentence i sWithIds
                                                  putStrLn $ renderAsTable results
                                                  rawchoice <- getLine
                                                  case readMaybe rawchoice of
                                                      Nothing -> case rawchoice of
                                                                     "i" -> return $ Word w
                                                                     "m" -> errorCase -- TODO
                                                                     "x" -> exitSuccess
                                                      Just ch -> do if ch >= 0 && ch < length results
                                                                    then return $ Word (fst $ results !! ch)
                                                                    else errorCase
        highlightInSentence :: Int -> [(Int, SentencePart)] -> String
        highlightInSentence i = ('\n':) . concatMap (\(i', p) -> case p of Punctuation w' -> faintText w'
                                                                           Word w'        -> if i /= i' then faintText w' else w')
        renderAsTable :: [Result] -> String
        renderAsTable = ("\n" ++)
                      . (++ faintText "i ignore\nm more suggestions\nx exit/abort")
                      . concatMap (\(i, (s, c)) -> faintText (show i ++ ". ")
                                                ++ s
                                                ++ faintText (" (" ++ show c ++ ")\n"))
                      . zip [0..]

-- | Undo 'segmentText' and restore a 'Sentence' list into plain text.
renderSentences :: [Sentence] -> String
renderSentences = foldr (\s acc -> concatMap renderSentencePart s ++ acc) ""
    where renderSentencePart p = case p of Word s -> s
                                           Punctuation s -> s

main = do ts <- fromWLFile "/home/sjm/downloads/aspell-dump-expand-de_DE.utf8.txt"
          putStrLn $ (show length $ toList ts) ++ " words loaded." -- Cheap deepseq. Also nice to know.
          result <- correctText "- ... das ist (natürlich) ein Test, welcher Dinge? (Naja...) tun will...! hachja" ts
          putStrLn $ "RESULT: " ++ result
