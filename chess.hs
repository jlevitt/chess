import Data.Char (chr, ord)

data Position = Position { x :: Int, y :: Int }
data Vector = Vector { xStep :: (Int -> Int), yStep :: (Int -> Int) }

instance Show Position where
    show p = let (file, rank) = translate p in file:show rank

pawnMoves = [Vector (+0) (+1), Vector (+0) (+2)]
knightMoves = [Vector (+(-2)) (+1), Vector (+(-1)) (+2), Vector (+1) (+2), Vector (+2) (+1), Vector (+2) (+(-1)), Vector (+1) (+(-2)), Vector (+(-1)) (+(-2)), Vector (+(-2)) (+(-1))]

translate :: Position -> (Char, Int)
translate (Position x y) = (chr (65 + x), y + 1)

getPosition :: Char -> Int -> Position
getPosition x y = Position ((ord x) - 65) (y - 1)

move :: Position -> Vector -> Position
move (Position x y) (Vector xStep yStep) = Position (xStep x) (yStep y)

legalPawnMoves :: Position -> [Position]
legalPawnMoves piece = map (move piece) pawnMoves

legalKnightMoves :: Position -> [Position]
legalKnightMoves piece = map (move piece) knightMoves
