{-# LANGUAGE TemplateHaskell #-}
module Board where

import qualified Data.Map as M

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
data Coordinate x y = Coordinate { _x :: x, _y :: y } deriving (Show, Eq, Ord)
data Board = Board { _pieces :: M.Map (Coordinate Char Int) Piece } deriving (Eq)

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

boardCoordinates :: [Coordinate Char Int]
boardCoordinates = [Coordinate x y | y <- [8,7..1], x <- ['a'..'h']]

instance Show Board where
  show (Board b) = foldr (\c r -> (maybe " " show (c `M.lookup` b)) ++
                                  ( if view x c == 'h' && view y c /= 1 then "\n" else "") ++
                                  r)
                   "" boardCoordinates

pawnLine :: Int -> Color -> [(Coordinate Char Int, Piece)]
pawnLine l c = map (\r -> (Coordinate r l, Piece c Pawn)) ['a'..'h']

piecesLine :: Int -> Color -> [(Coordinate Char Int, Piece)]
piecesLine l c = zipWith (\t r -> (Coordinate r l, Piece c t))
                 [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
                 ['a'..'h']

initBoard :: Board
initBoard = Board ( M.fromList $
                    piecesLine 8 Black ++
                    pawnLine 7 Black ++
                    pawnLine 2 White ++
                    piecesLine 1 White
                  )
