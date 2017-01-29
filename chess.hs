import Data.Char (chr, ord, toUpper)

data Square = Square { x :: Int, y :: Int }
data Vector = Vector { xStep :: (Int -> Int), yStep :: (Int -> Int) }

instance Show Square where
    show s = let (file, rank) = translate s in file:show rank

translate :: Square -> (Char, Int)
translate (Square x y) = (chr (65 + x), y + 1)

mkSquare :: String -> Square
mkSquare (file:rank:[]) = Square x y
    where x = (ord . toUpper) file - 65
          y = ord rank - 49

onBoard :: Square -> Bool
onBoard (Square x y ) = x >=0 && x <= 7 && y >= 0 && y <= 7

applySteps :: [Vector] -> Square -> [Square]
applySteps moves square = map (move square) moves

move :: Square -> Vector -> Square
move (Square x y) (Vector xStep yStep) = Square (xStep x) (yStep y)

pawnSteps = [Vector (+0) (+1), Vector (+0) (+2)]
knightSteps = [Vector (+(-2)) (+1), Vector (+(-1)) (+2), Vector (+1) (+2), Vector (+2) (+1), Vector (+2) (+(-1)), Vector (+1) (+(-2)), Vector (+(-1)) (+(-2)), Vector (+(-2)) (+(-1))]
unboundedBishopSteps = concat (map (\x -> [Vector (+x) (+x), Vector (+x) (+(-x)), Vector (+(-x)) (+x), Vector (+(-x)) (+(-x))]) [1..])

legalPawnMoves :: Square -> [Square]
legalPawnMoves = applySteps pawnSteps

legalKnightMoves :: Square -> [Square]
legalKnightMoves = applySteps knightSteps

boundedBishopMoves :: Square -> [Square]
boundedBishopMoves square = takeValid (applySteps unboundedBishopSteps square)
    where takeValid = takeWhile onBoard
