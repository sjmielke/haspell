-- | Haspell lets the user correct a text read form a file
-- interactively by comparing words (using the minimum edit distance)
-- to suggestions from a word list.

module Main (
    correctText,
    CLIFlags(..),
    main
    ) where

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

-- | A part of a sentence is either a 'Word' (i.e. something
-- we want to correct) or 'Punctuation' (something we ignore).
data SentencePart = Word String | Punctuation String deriving (Show)
-- | A sentence is a list of 'SentencePart's
-- (including punctuation and space at the end).
type Sentence = [SentencePart]

-- | Callable entry point for interactive text correction.
correctText :: WTrie -> String -> Bool -> IO String
correctText ts txt compat = do newSentences <- mapM (correctSentence ts compat) (segmentText txt)
                               return $ reconstructSentences newSentences

-- | Primitive segmentation of a given user input into 'Sentence's
-- containing 'SentencePart's ('Word's and 'Punctuation')
segmentText :: String -> [Sentence]
segmentText = map segmentSentence . splitOnSubSeqs [". ", "! ", "? ", ".\t", "!\t", "?\t", ".\n", "!\n", "?\n"] . (:[])
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
                        errorCase = do putStrLn $ invertText compat "Incorrect choice, please try again."
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

-- | The user can specify various flags over the CLI.
data CLIFlags = CLIFlags { wordlist :: FilePath
                         , userFile :: FilePath
                         , compatRender :: Bool
                         , outFile :: FilePath
                         } deriving Show

-- | Runs the CLI built using the @argparser@ package.
main :: IO ()
main = runApp app{getAppVersion = Just " 1.0 beta"} runWithFlags
    where
        app = mkDefaultApp (CLIFlags `parsedBy` reqPos "wordlist" `Descr` "Word list file name"
                                     `andBy` reqPos "text" `Descr` "Input file name"
                                     `andBy` boolFlag "compat" `Descr` "Compatibility rendering mode"
                                     `andBy` optFlag "" "outfile" `Descr` "Output file name"
                           )
                           "Haspell"
              `setAppDescr` "(Haskell spell correction based on minimum edit distance calculation)"
              `setAppEpilog` "Developed for a university course by Sebastian J. Mielke 2014"
        runWithFlags myFlags = do putStrLn "Now loading word list..."
                                  -- openFile already does nice error handling.
                                  ts <- fromWLFile $ wordlist myFlags
                                  -- Cheap deepseq. Also nice to know.
                                  putStrLn $ (show . length $ toList ts) ++ " words were loaded."
                                  
                                  putStrLn "Now loading input text..."
                                  -- openFile already does nice error handling.
                                  userinput <- readFile $ userFile myFlags
                                  
                                  -- Run interactive correction session.
                                  result <- correctText ts userinput (compatRender myFlags)
                                  
                                  -- Write corrected text to filename.corrected or user-specified location.
                                  let outFilePath = case outFile myFlags of
                                                      ""       -> (userFile myFlags ++ ".corrected")
                                                      filename -> filename
                                  putStrLn $ "Saving corrected text to \"" ++ outFilePath ++ "\"..."
                                  writeFile outFilePath result
                                  putStrLn "All done. So long, and thanks for all the fish!"
