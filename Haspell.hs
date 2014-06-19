module Haspell where

-- | Wrap text with given ANSI code and reset code to change display properties.
-- (see http://en.wikipedia.org/wiki/ANSI_escape_code )
ansiWrap :: Int -> String -> String
ansiWrap i = (("\ESC[" ++ (show i) ++ "m")++) . (++"\ESC[0m")

-- | Insert ANSI codes to display the argument with inverted colors.
invertText :: String -> String
invertText = ansiWrap 7

-- | Insert ANSI codes to display the argument in fainter font.
faintText :: String -> String
faintText = ansiWrap 2
