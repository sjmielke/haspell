module Main where

import Data.Default (def)
import Data.Char (isLetter)
import Data.List.Split (split, whenElt, dropBlanks, keepDelimsR, onSublist)
import Text.Read (readMaybe)
import System.Exit (exitSuccess)
import System.Console.ArgParser (ParserSpec, Descr(..), parsedBy, andBy, reqPos, boolFlag, optFlag, setAppEpilog, setAppDescr, mkDefaultApp, runApp, getAppVersion)

import WTrie
import TrieMED

-- | Wrap text with given ANSI code and reset code to change display properties, unless the comatibility option is set.
-- (see @http://en.wikipedia.org/wiki/ANSI_escape_code@ )
ansiWrap :: Bool -> Int -> String -> String
ansiWrap compat i = if compat then id else (("\ESC[" ++ (show i) ++ "m")++) . (++"\ESC[0m")

-- | Insert ANSI codes to display the argument with inverted colors, unless the comatibility option is set.
invertText :: Bool -> String -> String
invertText compat = ansiWrap compat 7

-- | Insert ANSI codes to display the argument underlined, unless the comatibility option is set.
underlineText :: Bool -> String -> String
underlineText compat = ansiWrap compat 4

-- | Insert ANSI codes to display the argument in fainter font, unless the comatibility option is set.
faintText :: Bool -> String -> String
faintText compat = ansiWrap compat 2

data SentencePart = Word String | Punctuation String deriving (Show)
type Sentence = [SentencePart]

-- | Callable entry point for interactive text correction.
correctText :: WTrie -> String -> Bool -> IO String
correctText ts txt compat = do newSentences <- mapM (correctSentence ts compat) (segmentText txt)
                               return $ reconstructSentences newSentences

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
        segmentSentence = map (\s@(x:xs) -> if isLetterish x then Word s else Punctuation s)
                        . split (dropBlanks $ whenElt (not . isLetterish))
        isLetterish :: Char -> Bool
        isLetterish c = (c == '\'') || (isLetter c)

-- | Interactively correct a segmented sentence using a given 'WTrie'
-- (optionally using a more compatible rendering mode).
correctSentence :: WTrie -> Bool -> Sentence -> IO Sentence
correctSentence ts compat s = mapM (correctSentencePart 10) sWithIds
    where
        sWithIds = zip [0..] s
        correctSentencePart :: Int -> (Int, SentencePart) -> IO SentencePart
        correctSentencePart _ (i, p@(Punctuation _)) = return p
        correctSentencePart numberOfSuggestions (i, Word w) =
            do if ts `contains` w
               then return $ Word w
               else do
                    let results = calcMEDs numberOfSuggestions w ts
                        errorCase = do putStrLn $ invertText compat "What the fuck is wrong with you? Try again."
                                       correctSentencePart numberOfSuggestions (i, Word w)
                    putStrLn $ highlightInSentence i sWithIds
                    putStrLn $ renderAsTable results
                    rawchoice <- getLine
                    case readMaybe rawchoice of
                        Nothing -> case rawchoice of
                                       "i" -> return $ Word w
                                       "m" -> correctSentencePart (numberOfSuggestions + 10) (i, Word w)
                                       "x" -> exitSuccess
                                       _   -> errorCase
                        Just ch -> do if ch >= 0 && ch < length results
                                      then return $ Word (fst $ results !! ch)
                                      else errorCase
        highlightInSentence :: Int -> [(Int, SentencePart)] -> String
        highlightInSentence i = ('\n':) . concatMap (\(i', p) ->
                                            case p of Punctuation w' -> faintText compat w'
                                                      Word w'        -> if i /= i'
                                                                        then faintText compat w'
                                                                        else if compat
                                                                             then "   > " ++ w' ++ " <   "
                                                                             else w')
                                        . take 21 -- Truncate long sentences
                                        . drop (i - 10)
        renderAsTable :: [Result] -> String
        renderAsTable = ("\n" ++)
                      . (++ faintText compat "i ignore\nm more suggestions\nx exit/abort")
                      . concatMap (\(i, (s, c)) -> faintText compat (show i ++ ". ")
                                                ++ s
                                                ++ faintText compat (" (" ++ show c ++ ")\n"))
                      . zip [0..]

-- | Undo 'segmentText' and restore a 'Sentence' list into plain text.
reconstructSentences :: [Sentence] -> String
reconstructSentences = foldr (\s acc -> concatMap reconstructSentencePart s ++ acc) ""
    where reconstructSentencePart p = case p of Word s -> s
                                                Punctuation s -> s

data CLIFlags = CLIFlags { wordlist :: FilePath
                         , userFile :: FilePath
                         , compatRender :: Bool
                         , outFile :: FilePath
                         } deriving Show

main :: IO ()
main = runApp app{getAppVersion = Just " 1.0 alpha"} runWithFlags
    where app = mkDefaultApp (CLIFlags `parsedBy` reqPos "wordlist" `Descr` "Word list file name"
                                          `andBy` reqPos "text" `Descr` "Input file name"
                                          `andBy` boolFlag "compat" `Descr` "Compatibility rendering mode"
                                          `andBy` optFlag "" "outfile" `Descr` "Output file name"
                             )
                             "Haspell"
                `setAppDescr` "(Haskell spell correction based on minimum edit distance calculation)"
                `setAppEpilog` "Developed for a university course by Sebastian J. Mielke 2014"

runWithFlags :: CLIFlags -> IO ()
runWithFlags myFlags = do -- openFile already does nice error handling.
                          ts        <- fromWLFile $ wordlist myFlags
                          userinput <-   readFile $ userFile myFlags
                          -- Cheap deepseq. Also nice to know.
                          putStrLn $ (show . length $ toList ts) ++ " words loaded."
                          -- Run interactive correction session.
                          result <- correctText ts userinput (compatRender myFlags)
                          -- Write corrected text to filename.corrected or user-specified location.
                          case outFile myFlags of
                              ""       -> writeFile (userFile myFlags ++ ".corrected") result
                              filename -> writeFile filename result


{-

Potenzielle TODOs
-----

Wenn ihr euer Programm möglichst flexibel gestalten wollt, ist es vielleicht 
sinnvoll, nur Zeichen, die im Wörterbuch vorkommen, als Zeichen innerhalb von 
Worten zu akzeptieren und alle anderen Zeichen als Worttrenner zu betrachten.

Besserer Einfügealgorithmus, Stackkram und so.

Vorschläge, die gleich anfangen (oder case sensitive kram) bevorzugen

-}
