{-# LANGUAGE FlexibleInstances #-}
module Board where

import qualified Data.Map as M
import qualified Data.List as L

import Data.Ord
import Data.Functor
import Data.Monoid
import Control.Monad

repr :: M.Map PieceType (Char, Char)
repr = M.fromList $ [(King,   ('♔','♚')),
                     (Queen,  ('♕','♛')),
                     (Rook,   ('♖','♜')),
                     (Bishop, ('♗','♝')),
                     (Knight, ('♘','♞')),
                     (Pawn,   ('♙','♟'))]

data Color = Black | White deriving (Eq)
data PieceType = Pawn | Bishop | King | Queen | Rook | Knight deriving (Eq, Ord)
data Piece = Piece Color PieceType deriving (Eq)

getPieceRepr :: Piece -> String
getPieceRepr (Piece c t) = (cget $ repr M.! t) : " "
    where cget = if c == Black then snd else fst

instance Show Piece where
  show = getPieceRepr

class Board b p where
  initBoard :: b p
  coordinates :: b p -> [p] -- TODO: find more general data structure than List (Traversable, Functor, Applicative?)
  get         :: b p -> p -> Maybe Piece

newtype Board8x8 p = Board8x8 { getBoard :: M.Map p Piece } deriving (Eq)

pawnLine :: Int -> Color -> [((Char, Int), Piece)]
pawnLine l c = map (\r -> ((r,l), Piece c Pawn)) ['a'..'h']

piecesLine :: Int -> Color -> [((Char, Int), Piece)]
piecesLine l c = zipWith (\t r -> ((r,l), Piece c t))
                 [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
                 ['a'..'h']

instance Board Board8x8 (Char, Int) where
  initBoard = Board8x8 ( M.fromList $
                         piecesLine 8 Black ++
                         pawnLine 7 Black ++
                         pawnLine 2 White ++
                         piecesLine 1 White
                       )
  b `get` pos = pos `M.lookup` getBoard b
  coordinates b = [ (x,y) | y <- [8,7..1], x <- ['a'..'h'] ]
  -- a `applyMove` m = undefined

-- instance Show Board8x8 where
--   show b = foldr (\c r -> (maybe " " show (b `get` c)) ++ ( if fst c == 'h' then "\n" else "") ++ r) "" (coordinates b)

setBackground :: Color -> String -> String
setBackground c s = case c of
  Black -> "\ESC[1;43m" ++ s ++ "\ESC[0m"
  White -> "\ESC[1;40m" ++ s ++ "\ESC[0m"
