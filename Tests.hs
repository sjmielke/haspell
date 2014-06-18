module Main where

import Data.List ((\\))
import qualified Data.Set as S

import System.Environment (getArgs)

import WTrie
import Spell

testWL :: [String] -> IO ()
testWL wl_dup = let wl = nub' S.empty wl_dup
                    wl' = toList . fromList $ wl
                    diff = (wl \\ wl') in -- ++ (wl' \\ wl) in
                putStrLn $ if null diff then "All words were accurately reproduced."
                                        else "ERROR in exact word reproduction, diff:" ++ show diff
    where
        nub' _ [] = []
        nub' s (x:xs) = if S.member x s then nub' s xs
                                        else x : nub' (S.insert x s) xs


testFile :: String -> IO ()
testFile f = do rawwords <- readFile f
                testWL $ words rawwords

testExample :: IO ()
testExample = do let res = calcMED "Lit" $ fromList ["Lot", "Lose", "Hose", "Los"]
                 putStrLn $ if lookup "Lot" res == Just 1
                            && lookup "Los" res == Just 2
                            && lookup "Lose" res == Just 3
                            && lookup "Hose" res == Just 4 then "All four MEDs were correct."
                                                           else "ERROR in checking MEDs!"

main = do ts <- fromFile "/home/sjm/downloads/aspell-dump-expand-de_DE.utf8.txt"
          let wordnum = length $ toList ts -- Cheap deepseq. Also nice to know.
          putStrLn $ show wordnum ++ " words loaded."
          -- (uword : _) <- getArgs
          let simpleTest w = show $ calcMED w ts
          let printSimpletest = putStrLn . simpleTest
          -- mapM printSimpletest ["sfalihfaiwuehfliwauehfaiwfa", "ajfalwfaheofifmafowjfaiofja", "iwaefiehfalierjgmvcsafsefrf", "aioewjfoasmcoreijfsoerigmer", "asoiejfadorjgaormfairjfoeig", "wefaiwhefiascaimifaloifricm", "oeiafjigrofimaeorijfseorjgj", "aoiejfmosimfcijfloirajfsirg", "maoirjfoaaergaeergjfsfsafsm", "fliwaufsaedfsdfsadfehfaiwfa", "fmaforegwefawefwaefjfaiofja", "rjgmvwefasdasdfasfcsafsefrf", "reijfdefasfaefawefsoerigmer", "aormfaefrthfaefaefairjfoeig", "aimifaefaweffsawefaloifricm", "aeorijawrsthaewfgerfseorjgj", "ijfloirathsrhrsthrztqjfsirg", "maoirjfoaersfawgaageraergjm"]
          mapM printSimpletest ["Test", "Absolue", "Rekonstruktion", "a", "Versicherungskaufmann", "b"]
          -- mapM printSimpletest ["awejoawg", "sefawf", "dsefafg", "fiewajfwrg", "fwaeifa"]
          -- interact (concatMap simpleTest . lines)
