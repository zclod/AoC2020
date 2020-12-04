{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Standard input
import           Protolude hiding (interact,many)
import qualified Turtle as T
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer
import           Data.Text (Text)
import           Data.Void
import           Data.Text.IO (interact)
-----------------


type Parser = Parsec Void Text

difficulty = T.switch "hard" 'd' "diffuculty level"

mkSolution :: Show b => Parser a -> (a -> b) -> Text -> Text
mkSolution parser solution i = show $ solution <$> parseMaybe parser i

easy = mkSolution easyparser easysolution

hard = mkSolution easyparser hardsolution

main = do
    isHard <- T.options "Advent of Code 2020" difficulty
    if isHard then
        interact hard
    else
        interact easy

--------------------------------------------------------------------

easysolution :: [Int] -> [Int]
easysolution i = take 1 $ filter ((/=) 0) $ do
    a <- i
    b <- i
    if (a+b == 2020) then pure (a*b) else pure 0

hardsolution :: [Int] -> [Int]
hardsolution i = take 1 $ filter ((/=) 0) $ do
    a <- i
    b <- i
    c <- i
    if (a+b+c == 2020) then pure (a*b*c) else pure 0

easyparser :: Parser [Int]
-- easyparser = many $ decimal <* eol
easyparser = sepEndBy decimal eol
