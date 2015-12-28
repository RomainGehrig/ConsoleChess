{-# LANGUAGE TemplateHaskell #-}
module Board where

import qualified Data.Map as M

import Data.List
import Data.Bits (xor)
import Control.Lens

repr :: M.Map PieceType (Char, Char)
repr = M.fromList $ [(King,   ('♔','♚')),
                     (Queen,  ('♕','♛')),
                     (Rook,   ('♖','♜')),
                     (Bishop, ('♗','♝')),
                     (Knight, ('♘','♞')),
                     (Pawn,   ('♙','♟'))]

data Color = Black | White deriving (Eq, Show)
data PieceType = Pawn | Bishop | King | Queen | Rook | Knight deriving (Eq, Ord, Show)
data Piece = Piece { _color :: Color
                   , _pieceType :: PieceType } deriving (Eq)
data Coordinate a = Coordinate { _x :: a, _y :: a } deriving (Show, Eq, Ord)
data Board = Board { _pieces :: M.Map (Coordinate Int) Piece } deriving (Eq)

data BackgroundColor = Light | Dark deriving (Show, Eq)
backgroundColor :: BackgroundColor
backgroundColor = Dark

instance Show Piece where
  show = getPieceRepr backgroundColor

getPieceRepr :: BackgroundColor -> Piece -> String
getPieceRepr bg (Piece c t) = (cget $ repr M.! t) : " "
  where cget = if (c == Black) `xor` (bg == Dark) then snd else fst

makeLenses ''Piece
makeLenses ''Coordinate
makeLenses ''Board

boardCoordinates :: [Coordinate Int]
boardCoordinates = [Coordinate x y | y <- [1..8], x <- [1..8]]

instance Show Board where
  show (Board b) = foldr (\c r -> (maybe " " show (c `M.lookup` b)) ++
                                  (if view x c == 8 && view y c /= 1 then "\n" else "") ++
                                  r)
                   "" $ sortOn (\e -> (negate $ e ^. y, e ^. x)) boardCoordinates

pawnLine :: Int -> Color -> [(Coordinate Int, Piece)]
pawnLine l c = map (\r -> (Coordinate r l, Piece c Pawn)) [1..8]

piecesLine :: Int -> Color -> [(Coordinate Int, Piece)]
piecesLine l c = zipWith (\t r -> (Coordinate r l, Piece c t))
                 [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
                 [1..8]

initBoard :: Board
initBoard = Board ( M.fromList $
                    piecesLine 8 Black ++
                    pawnLine 7 Black ++
                    pawnLine 2 White ++
                    piecesLine 1 White
                  )
