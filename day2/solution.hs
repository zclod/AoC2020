{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Standard input
import           Protolude hiding (interact,many)
import qualified Turtle as Tr
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer hiding (space)
import           Data.Text (Text, pack)
import qualified Data.Text as T
import           Data.Void
import           Data.Text.IO (interact)
-----------------


type Parser = Parsec Void Text

difficulty = Tr.switch "hard" 'd' "diffuculty level"

mkSolution :: Show b => Parser a -> (a -> b) -> Text -> Text
mkSolution parser solution i = show $ solution <$> parseMaybe parser i

easy = mkSolution easyparser easysolution

hard = mkSolution easyparser hardsolution

main = do
    isHard <- Tr.options "Advent of Code 2020" difficulty
    if isHard then
        interact hard
    else
        interact easy

--------------------------------------------------------------------
data Policy = P { min :: Int
                , max :: Int
                , letter :: Char
                }

policy :: Parser Policy
policy = P <$> (decimal <* char '-') <*> (decimal <* space) <*> (letterChar <* char ':' <* space)

password :: Parser Text
password = pack <$> many alphaNumChar

entry :: Parser (Policy, Text)
entry = (,) <$> policy <*> password

easyparser :: Parser [(Policy, Text)]
easyparser = sepEndBy entry eol

easysolution :: [(Policy,Text)] -> Int
easysolution i = correctPassNum
    where checkPass ((P min max l), p) = let s = T.length $ T.filter ((==) l) p in s <= max && s >= min
          correctPassNum = length $ filter checkPass i

hardsolution :: [(Policy,Text)] -> Int
hardsolution i = correctPassNum
    where checkPass ((P min max l), p) = let l1 = T.index p (min - 1)
                                             l2 = T.index p (max - 1)
                                          in (l1 == l && l2 /= l) || (l1 /= l && l2 == l)
          correctPassNum = length $ filter checkPass i
