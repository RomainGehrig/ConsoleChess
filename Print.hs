{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, TypeSynonymInstances #-}
import Data.Monoid
import Data.Ord (comparing, compare)
import Data.Function (on)
import Data.List (intersperse, groupBy, sortBy)
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

-- class Show a                where smartShow :: a -> String
-- instance Show String        where smartShow = id
-- instance (Show a) => Show a where smartShow = show


data SquareColor = BlackS | WhiteS deriving (Eq)

data Square s a = Square { element :: a
                         , infos :: [a]
                         , color :: SquareColor}

newtype Line s a = Line { getSquareLine :: [Square s a] }
newtype Matrix s a = Matrix { getMatrix :: [Line s a] }

createSquare :: Space s => s -> SquareColor -> a -> Square s a
createSquare s c a = Square { element = a, infos = [], color = c }

appendSquare,addSquare :: (Space s) => Square s a -> Line s a -> Line s a
appendSquare squ l = Line $ getSquareLine l ++ [squ]
addSquare squ l = Line $ squ : (getSquareLine l)

appendLine,addLine :: Space s => Line s a -> Matrix s a -> Matrix s a
appendLine l m = Matrix $ getMatrix m ++ [l]
addLine l m = Matrix $ l : (getMatrix m)

completeList :: Int -> a -> [a] -> [a]
completeList 0 _      _ = []
completeList i d     [] = d : completeList (i-1) d []
completeList i d (x:xs) = x : completeList (i-1) d xs

spaceFromSquare :: Space s => Square s a -> Int
spaceFromSquare sq = getSpace $ (undefined :: Square s a -> s) $ sq

-- smartShow :: Show a => a -> String
-- smartShow (s :: String) = s
-- smartShow o             = show o

toLst :: (Space s, Show a) => Square s a -> [String]
toLst sqr@(Square e inf _) = [ chunk | chunk <- chunksOf side finalLst]
  where space = spaceFromSquare sqr
        side = space*2 + 1
        total = side*side - 1
        infoLst = completeList total ' ' (concatMap show inf)
        finalLst = (\(f, r) -> f ++ show e ++ r) $ splitAt (total `div` 2) infoLst

instance (Space s, Show a) => Show (Line s a) where
  show (Line ss) = concat $ intersperse "\n" $ foldr1 (zipWith (++)) $ map toLst ss

instance (Space s, Show a) => Show (Matrix s a) where
  show (Matrix ls) = concatMap show ls

testLine :: Line A0 String
testLine = Line $ map (createSquare (undefined :: A0) WhiteS) ["a", "b", "c"]

boardToMatrix :: (Board a, Space s) => a -> s -> Matrix s String
boardToMatrix b s = Matrix
                    $ map (Line . map (\p -> createSquare s WhiteS $ maybe "-" (getPieceRepr) (b `get` p)))
                    $ groupBy ((==) `on` snd)
                    $ sortBy (comparing snd `mappend` (flip compare `on` fst))
                    $ coordinates b
