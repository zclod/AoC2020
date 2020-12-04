{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Standard input
import           Protolude hiding (interact,many,some)
import qualified Turtle as Tr
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer hiding (space)
import           Data.Text (Text, pack)
import qualified Data.Text as T
import           Data.Void
import           Data.Text.IO (interact)
-----------------

import           Data.Sequence (Seq)
import qualified Data.Sequence as S
import           Data.Functor.Foldable


type Parser = Parsec Void Text

difficulty = Tr.switch "hard" 'd' "diffuculty level"

mkSolution :: Show b => Parser a -> (a -> b) -> Text -> Text
mkSolution parser solution i = show $ solution <$> parseMaybe parser i

easy = mkSolution easyparser easysolution
-- easy = mkSolution easyparser identity

hard = mkSolution easyparser hardsolution'
-- hard = undefined

main = do
    isHard <- Tr.options "Advent of Code 2020" difficulty
    if isHard then
        interact hard
    else
        interact easy

--------------------------------------------------------------------
data Loc = Empty | Tree deriving (Eq, Show)
type Row = Seq Loc

pLoc :: Parser Loc
pLoc = choice
    [ Empty <$ char '.'
    , Tree <$ char '#']

pRow :: Parser Row
pRow = S.fromList <$> some pLoc

easyparser :: Parser [Row]
easyparser = sepEndBy pRow eol

easysolution :: [Row] -> Int
-- easysolution i = snd $ foldl f ((0,0),0) i
-- easysolution i = foldl f ((0,0),0) i
--         where f = mkfold (3,1)
easysolution i = hylo alg (mkcoalg (3,1)) ((0,0), i)

mkcoalg (dx, dy) ((x, y), rs) = case rs of
                      [] -> Nil
                      r:rss -> let l = S.length r
                                   x' = if y == 0 then mod (x+dx) l else x
                                   y' = mod (y+1) dy
                                in Cons ((x,y), r) ((x',y'), rss)

alg Nil = 0
alg (Cons ((x,y),r) acc) = if (S.index r x) == Tree && y == 0 then acc + 1 else acc

mkfold (x,y) = f
    -- where f r ((px, py), acc) = let l = S.length r
    where f ((px, py), acc) r = let l = S.length r
                                    px' = if (py == 0) then mod (px + x) l else px
                                    py' = mod (py + 1) y
                               in
                                  if (S.index r px) == Tree && py == 0
                                     then ((px',py'),acc+1)
                                     else ((px',py'),acc)

hardsolution :: [Row] -> Int
hardsolution i = product $ snd <$> trees
    where fs = mkfold <$> [(1,1), (3,1), (5,1), (7,1), (1,2)]
          trees = foldl <$> fs <*> [((0,0),0)] <*> [i]


hardsolution' :: [Row] -> Int
hardsolution' i = product $ trees
    where fs = mkcoalg <$> [(1,1), (3,1), (5,1), (7,1), (1,2)]
          trees = hylo <$> [alg] <*> fs <*> [((0,0), i)]
