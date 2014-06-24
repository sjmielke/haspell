module Main where

import Data.Default (def)
import Data.Char (isLetter)
import Data.List.Split (split, whenElt, dropBlanks, keepDelimsR, onSublist)
import Text.Read (readMaybe)
import System.Exit (exitSuccess)
import System.Console.ArgParser (ParserSpec, Descr(..), parsedBy, andBy, reqPos, boolFlag, setAppEpilog, setAppDescr, mkDefaultApp, runApp)

import WTrie
import TrieMED

-- | Wrap text with given ANSI code and reset code to change display properties.
-- (see @http://en.wikipedia.org/wiki/ANSI_escape_code@ )
ansiWrap :: Int -> String -> String
ansiWrap i = (("\ESC[" ++ (show i) ++ "m")++) . (++"\ESC[0m")

-- | Insert ANSI codes to display the argument with inverted colors.
invertText :: String -> String
invertText = ansiWrap 7

-- | Insert ANSI codes to display the argument underlined.
underlineText :: String -> String
underlineText = ansiWrap 4

-- | Insert ANSI codes to display the argument in fainter font.
faintText :: String -> String
faintText = ansiWrap 2

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
        segmentSentence = map (\s@(x:xs) -> if isLetter x then Word s else Punctuation s)
                        . split (dropBlanks $ whenElt (not . isLetter))

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
                        errorCase = do putStrLn $ invertText "What the fuck is wrong with you? Try again."
                                       correctSentencePart numberOfSuggestions (i, Word w)
                    putStrLn $ highlightInSentence i sWithIds
                    putStrLn $ renderAsTable results
                    rawchoice <- getLine
                    case readMaybe rawchoice of
                        Nothing -> case rawchoice of
                                       "i" -> return $ Word w
                                       "m" -> correctSentencePart (numberOfSuggestions + 10) (i, Word w)
                                       "x" -> exitSuccess
                        Just ch -> do if ch >= 0 && ch < length results
                                      then return $ Word (fst $ results !! ch)
                                      else errorCase
        highlightInSentence :: Int -> [(Int, SentencePart)] -> String
        highlightInSentence i = ('\n':) . concatMap (\(i', p) -> case p of Punctuation w' -> faintText w'
                                                                           Word w'        -> if i /= i'
                                                                                             then faintText w'
                                                                                             else if compat
                                                                                                  then invertText w'
                                                                                                  else w')
                                        . take 21 -- Truncate long sentences
                                        . drop (i - 10)
        renderAsTable :: [Result] -> String
        renderAsTable = ("\n" ++)
                      . (++ faintText "i ignore\nm more suggestions\nx exit/abort")
                      . concatMap (\(i, (s, c)) -> faintText (show i ++ ". ")
                                                ++ (if compat then underlineText s else s)
                                                ++ faintText (" (" ++ show c ++ ")\n"))
                      . zip [0..]

-- | Undo 'segmentText' and restore a 'Sentence' list into plain text.
reconstructSentences :: [Sentence] -> String
reconstructSentences = foldr (\s acc -> concatMap reconstructSentencePart s ++ acc) ""
    where reconstructSentencePart p = case p of Word s -> s
                                                Punctuation s -> s

data CLIFlags = CLIFlags { wordlist :: FilePath
                         , userFile :: FilePath
                         , compatRender :: Bool
                         } deriving Show

main :: IO ()
main = runApp ( mkDefaultApp (CLIFlags `parsedBy` reqPos "wordlist" `Descr` "Word list file name"
                                          `andBy` reqPos "text" `Descr` "Input file name"
                                          `andBy` boolFlag "compat" `Descr` "Compatibility rendering mode (invert and underline instead of faint text)"
                             )
                             "Haspell"
                `setAppDescr` "(Haskell spell correction based on minimum edit distance calculation)"
                `setAppEpilog` "Developed for a university course by Sebastian J. Mielke 2014")
              runWithFlags
          
runWithFlags :: CLIFlags -> IO ()
runWithFlags myFlags = do print myFlags
                          ts <- fromWLFile "/home/sjm/downloads/aspell-dump-expand-en.utf8.txt"
                          --putStrLn $ (show . length $ toList ts) ++ " words loaded." -- Cheap deepseq. Also nice to know.
                          result <- correctText ts "fajfaoirfjaerihgeairjfoerig- ... das ist (natürlich) ein Test, welcher Dinge? (Naja...) tun will...! hachja" (compatRender myFlags)
                          --result <- correctText "Ein extrem langer Satz enthält (wenig überraschend) sehr sehr viele Worte, weitaus mehr, als vielleicht sinnvoll wäre, gerade wenn man ein bisschen auf so durchaus relevante Dinge wie Lesbarkeit achten möchte - ob das sinnvoll ist, sei natürlich dahingestellt, ein Text kann ja auch durch seine erdrückende Größe Eindruck schinden, lesen wird doch ohnehin überbewertet." ts
                          putStrLn $ "RESULT: " ++ result
