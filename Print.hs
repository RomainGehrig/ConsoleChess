{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, TypeSynonymInstances #-}
import Data.Monoid
import Data.Ord (comparing, compare)
import Data.Function (on)
import Data.List (intersperse, intercalate, groupBy, sortBy)
import Data.List.Split (chunksOf)

import Board

-- | Represent the space around the "main piece" of a Square
data A0
data A1
data A2

class    Space a  where getSpace :: a -> Int
instance Space A0 where getSpace = const 0
instance Space A1 where getSpace = const 1
instance Space A2 where getSpace = const 2

data SquareColor = BlackS | WhiteS deriving (Eq)

data Square s a = Square { element :: a
                         , infos :: [a]
                         , color :: SquareColor}

newtype Line s a = Line { getSquareLine :: [Square s a] }
newtype Matrix s a = Matrix { getMatrix :: [Line s a] }

createSquare :: Space s => s -> SquareColor -> a -> Square s a
createSquare s c a = Square { element = a, infos = [], color = c }

-- appendSquare,addSquare :: (Space s) => Square s a -> Line s a -> Line s a
-- appendSquare squ l = Line $ getSquareLine l ++ [squ]
-- addSquare squ l = Line $ squ : (getSquareLine l)
--
-- appendLine,addLine :: Space s => Line s a -> Matrix s a -> Matrix s a
-- appendLine l m = Matrix $ getMatrix m ++ [l]
-- addLine l m = Matrix $ l : (getMatrix m)

completeList :: Int -> a -> [a] -> [a]
completeList 0 _      _ = []
completeList i d     [] = d : completeList (i-1) d []
completeList i d (x:xs) = x : completeList (i-1) d xs

spaceFromSquare :: Space s => Square s a -> Int
spaceFromSquare sq = getSpace $ (undefined :: Square s a -> s) $ sq

toLst :: (Space s, Show a) => a -> Square s a -> [[a]]
toLst d sqr@(Square e inf _) = [ chunk | chunk <- chunksOf side finalLst ]
  where space = spaceFromSquare sqr
        side = space*2 + 1
        total = side*side - 1
        infoLst = completeList total d inf
        finalLst = (\(f, r) -> f ++ [e] ++ r) $ splitAt (total `div` 2) infoLst

instance (Space s) => Show (Line s String) where
  show (Line ss) = intercalate "\n" $ map (concat) $ foldr1 (zipWith (++)) $ map (toLst " ") ss

instance (Space s) => Show (Matrix s String) where
  show (Matrix ls) = intercalate "\n" . map show $ ls

instance Show (Board8x8 (Char, Int)) where
  show = show . boardToMatrix (undefined :: A0)

-- TODO: add possibility to flip the change the orientation of the matrix
--       (black pieces at the bottom for instance)
boardToMatrix :: (Ord x, Ord y, Board b (x,y), Space s) => s -> b (x,y) -> Matrix s String
boardToMatrix s b = Matrix
                    $ map (Line . map (\p -> createSquare s WhiteS $ maybe " " show (b `get` p)))
                    $ groupBy ((==) `on` snd)
                    $ sortBy ((flip compare `on` snd) `mappend` comparing fst)
                    $ coordinates b
