module Chesskell where

-- View, Model, Update


-- MODEL
-- =============================================================================

-- Pieces

-- data or newtype?
-- newtype Pint = Pint Int | XInt Int
-- shallow?
data PieceType = Pawn | Rook | Knight | Bishop | King | Queen

-- pieceType' = ["Pawn", "Rook" {- ... -}]

data Player = Black | White

-- type Piece = (Player, PieceType)

data Piece' = MkPiece'
  { pieceType :: PieceType
  , player    :: Player
  -- , moved :: Bool
  }

-- pawn has moved if off starting rank (cant come home)
-- king harder
-- castles harder
-- Jess looked to Chess notation
-- "parse dont validate" Alexis King
  -- data Expr = Div Expr NotZero
  -- data NonEmpty a = Singleton a | Cons a (NonEmpty a)

rows = ['a'..'h']
cols = ['1'..'8']

data Row = A | B | C | D | E | F | G | H -- "file"
data Col = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 -- "rank"



-- UPDATE
-- =============================================================================


wp :: String -> (Player, PieceType)
wp (x:y:[]) = case (x,y) of
  ('W', 'P') -> (White, Pawn)