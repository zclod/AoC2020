{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Standard input
import           Protolude hiding (interact,many,some,try)
import qualified Turtle as Tr
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer hiding (space)
import           Data.Text (Text, pack)
import qualified Data.Text as T
import           Data.Void
import           Data.Text.IO (interact)
-----------------

import           Data.Char (isDigit, isHexDigit)

type Parser = Parsec Void Text

difficulty = Tr.switch "hard" 'd' "diffuculty level"

mkSolution :: Show b => Parser a -> (a -> b) -> Text -> Text
mkSolution parser solution i = show $ solution <$> parseMaybe parser i

easy = mkSolution easyparser easysolution
-- easy = mkSolution easyparser identity

hard = mkSolution easyparser hardsolution
-- hard = undefined

main = do
    isHard <- Tr.options "Advent of Code 2020" difficulty
    if isHard then
        interact hard
    else
        interact easy

--------------------------------------------------------------------
data PassportEntry = Byr Int | Iyr Int | Eyr Int |
                     Hgt Text | Hcl Text | Ecl Text |
                     Pid Text | Cid Text deriving (Show)

isEntryValid :: PassportEntry -> Bool
isEntryValid (Byr y) = y >= 1920 && y <= 2002
isEntryValid (Iyr y) = y >= 2010 && y <= 2020
isEntryValid (Eyr y) = y >= 2020 && y <= 2030
isEntryValid (Cid _) = True
isEntryValid (Pid p) = T.length p == 9 && T.all isDigit p
isEntryValid (Ecl c) = any (\e -> e) $ (==) <$> ["amb","blu","brn","gry","grn","hzl","oth"] <*> [c]
isEntryValid (Hcl c) = T.length c == 7 && let (hash,hex) = T.splitAt 1 c in hash == "#" && T.all isHexDigit hex
isEntryValid (Hgt h) = let (pHeight :: Parser (Int, Text)) = do h<-decimal
                                                                u<-some letterChar
                                                                pure (h,pack u) in
                           case (parseMaybe pHeight h) of
                             Just (he, "cm") -> he >= 150 && he <= 193
                             Just (he, "in") -> he >= 59 && he <= 76
                             _ -> False

type Passport = [PassportEntry]

pEntry :: Parser PassportEntry
pEntry = choice [ Byr <$> (string "byr:" *> decimal)
                , Iyr <$> (string "iyr:" *> decimal)
                , Eyr <$> (try $ string "eyr:" *> decimal)
                , Hgt <$> (try $ string "hgt:" >> T.pack <$> some (char '#' <|> alphaNumChar))
                , Hcl <$> (try $ string "hcl:" >> T.pack <$> some (char '#' <|> alphaNumChar))
                , Ecl <$> (try $ string "ecl:" >> T.pack <$> some (char '#' <|> alphaNumChar))
                , Pid <$> (string "pid:" >> T.pack <$> some (char '#' <|> alphaNumChar))
                , Cid <$> (string "cid:" >> T.pack <$> some (char '#' <|> alphaNumChar))
                ]

pPassport :: Parser Passport
pPassport = sepEndBy pEntry spaceChar

easyparser :: Parser [Passport]
easyparser = sepEndBy pPassport eol

easysolution :: [Passport] -> Int
easysolution i = length validPs
    where isCid pe = case pe of
                        Cid _ -> True
                        _ -> False
          isValid p = length p == 8 || (length p == 7 && all (not . isCid) p)
          validPs = filter isValid i


hardsolution :: [Passport] -> Int
hardsolution i = length validPs
    where isValid p = length p == 8 && all isEntryValid p || (length p == 7 && all (isEntryValid) p && all (not . isCid) p)
          isCid pe = case pe of
                        Cid _ -> True
                        _ -> False
          validPs = filter isValid i
