module Matrix where

newtype Matrix a = Matrix [[a]]

convolve :: Matrix a -> [[a]] -> [[a]]
convolve mat = map (map apply) . area mat $ padFrame mat

area :: Matrix a -> [[a]] -> [[Matrix a]]

apply :: Matrix a -> [[Matrix a]] -> a
apply mat xs = 



-- splitIntoNines calculates the neighborhood for each cell.
splitIntoNines :: World -> [[Neighborhood]]
splitIntoNines w = [[neighborhood x y w | x <- [1..width-2]] | y <- [1..height-2]] -- The offsets are to ignore the padding.
  where
    (width, height) = bounds w

-- padFrame adds a border with off-state cells around the world.
padFrame :: Matrix a -> [[a]] -> [[a]]
padFrame mat xs = frame : map (\x -> [Off] ++ x ++ [Off]) cs ++ [frame]
  where
    frame = replicate newWidth Off
    newWidth = size mat + (fst . bounds $ w)
padFrame w = error "not implemented"

size :: Matrix a -> Int
size mat = length mat
