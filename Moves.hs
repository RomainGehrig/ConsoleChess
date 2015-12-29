{-# LANGUAGE TemplateHaskell #-}
module Moves where

import qualified Data.Map as M
import Data.Maybe
import Data.Traversable
import Control.Lens

import Board
import Utils

data Move = Move { _from :: Coordinate Int, _to :: Coordinate Int }    deriving (Show,Eq)
data PlacedPiece = PlacedPiece { _piece :: Piece, _position :: Coordinate Int }  deriving (Show,Eq)
data GameState = GameState { _board :: Board, _moves :: [Move], _turn :: Color } deriving (Show,Eq)

makeLenses ''Move
makeLenses ''PlacedPiece
makeLenses ''GameState

initState :: GameState
initState = GameState initBoard [] White

isEatable :: Coordinate Int -> Color -> Board -> Bool
isEatable coord col board = maybe False (\p -> view color p /= col) piece
  where piece = coord `M.lookup` view pieces board

isOccupied :: Coordinate Int -> Board -> Bool
isOccupied coord board = isJust piece
  where piece = coord `M.lookup` view pieces board

isOnBoard :: Coordinate Int -> Bool
isOnBoard (Coordinate x y) = 1 <= x && x <= 8 && 1 <= y && y <= 8

-- | Create an infinite path in a certain direction from a starting point
pathFromStart :: Num a => (a, a) -> Coordinate a -> [Coordinate a]
pathFromStart direction start = iterate (updateCoordWithDirection direction) start

-- | Change a coordinate by adding a direction vector
updateCoordWithDirection :: Num a => (a,a) -> Coordinate a -> Coordinate a
updateCoordWithDirection (dx,dy) coord = coord & x +~ dx & y +~ dy

generates :: Piece -> [Coordinate Int -> [Coordinate Int]]
generates (Piece c Pawn)   = return $ \coord -> if c == Black then undefined else undefined
generates (Piece _ Bishop) = map pathFromStart directions
  where directions = (,) <$> [-1,1] <*> [-1,1]
generates (Piece _ King)   = map ((return .) . updateCoordWithDirection) directions
  where directions = filter (\(x,y) -> x /= 0 || y /= 0) $ (,) <$> [-1..1] <*> [-1..1]
generates (Piece _ Queen)  = map pathFromStart directions
  where directions = filter (\(x,y) -> x /= 0 || y /= 0) $ (,) <$> [-1..1] <*> [-1..1]
generates (Piece _ Rook)   = map pathFromStart directions
  where directions = [(0,1), (0,-1), (1,0), (-1,0)]
generates (Piece _ Knight) = map ((return .) . updateCoordWithDirection) directions
  where directions = [(x,y) | x <- dxy, y <- dxy, abs x /= abs y]
        dxy = [-2,-1,1,2]

getPiece :: Coordinate Int -> Board -> Maybe (PlacedPiece)
getPiece c (Board b) = c `M.lookup` b >>= \p -> Just $ PlacedPiece p c

movesForPiece :: PlacedPiece -> GameState -> [Move]
movesForPiece (PlacedPiece p pos) gs = map (Move pos) paths
  where b = gs ^. board
        paths = concatMap (\pathGen -> takeUntilWithHard hard soft $ pathGen pos) (generates p)
        hard = \coord -> not (isOnBoard coord) || (isOccupied coord b && not (isEatable coord (p ^. color) b))
        soft = \coord -> isEatable coord (p ^. color) b

possibleMoves :: GameState -> [Move]
possibleMoves gs = undefined
