module Main where

import Text.Show.Functions
import System.Random
import Data.List.Split
import Debug.Trace
import Graphics.Vty

width = 130
height = 20

onStr  = "o"
offStr = "·"

-- Cells can only be in two different states: on or off.
data Cell = On | Off deriving (Eq)

instance Show Cell where
  show On  = onStr
  show Off = offStr

instance Random Cell where
  random g = case random g of
    (True,  g') -> (On,  g')
    (False, g') -> (Off, g')

-- Law dictates how a neighborhood will change the middle cell.
type Law = Neighborhood -> Cell

-- Neighborhood are the 3x3 cells centered around the cell x:
-- ooo
-- oxo
-- ooo
type Neighborhood = [[Cell]]

-- Worlds are collection of cellular states.
data World = Closed  [[Cell]] -- A closed rectangle.
           | Plane   [[Cell]] -- An infinte plane.
           | Moebius [[Cell]] -- The north and south borders are mirror connected.
           | Torus   [[Cell]] -- The north and south borders are connected, as well as the east and west borders.

instance Show World where
  show = concatMap ((++ "\n") . concatMap show) . cells

-- Automaton enforces laws upon a world.
data Automaton = Automaton Law World

instance Show Automaton where
  show (Automaton law world) = show world

-- bounds returns the size of the world.
bounds :: World -> (Int,Int)
bounds w = (width, height)
  where
    (c:cs) = cells w
    width  = length c
    height = length (c:cs)

-- center returns the center cell of a neighborhood.
center :: Neighborhood -> Cell
center n = (n !! 1) !! 1

-- neighbors returns the number of alive neighbors in a neighborhood.
neighbors :: Neighborhood -> Int
neighbors n = surround - mid
  where
    surround = sum (map (length . filter isOn) n)
    mid
      | center n == On  = 1
      | center n == Off = 0

-- isOn returns true of the cell state is on, otherwise false.
isOn :: Cell -> Bool
isOn On = True
isOn Off = False

-- life are the collection of rules that makes up the game of life.
life :: Law
life n
  | center n == On && neighbors n < 2  = Off -- Underpopulation
  | center n == On && neighbors n == 2 = On  -- Lives on
  | center n == On && neighbors n == 3 = On  -- Lives on
  | center n == On && neighbors n > 3  = Off -- Overpopulation
-- Dead
  | center n == Off && neighbors n == 3 = On  -- Reproduction
  | otherwise                           = Off -- Stay dead

-- enforceLaw does one iteration of the cellular automaton.
enforceLaw :: Automaton -> Automaton
enforceLaw (Automaton l w) = Automaton l (applyLaw l w)

-- applyLaw applies the law to the world.
applyLaw :: Law -> World -> World
applyLaw l w@(Closed _) = Closed . map (map l) . neighborhoods $ w
applyLaw _ _ = error "not implemented"

-- neighborhoods returns the neighborhoods for each cell in the world.
neighborhoods :: World -> [[Neighborhood]]
neighborhoods = splitIntoNines . padFrame

-- splitIntoNines calculates the neighborhood for each cell.
splitIntoNines :: World -> [[Neighborhood]]
splitIntoNines w = [[neighborhood x y w | x <- [1..width-2]] | y <- [1..height-2]] -- The offsets are to ignore the padding.
  where
    (width, height) = bounds w

-- neighborhood returns the neighborhood around an (x,y) coordinate pair.
neighborhood :: Int -> Int -> World -> Neighborhood
neighborhood x y w@(Closed cs) = [[upLeft, up, upRight], [left, mid, right], [downLeft, down, downRight]]
  where
    upLeft
      | x-1 < 0        = Off
      | y-1 < 0        = Off
      | otherwise      = (cs !! (y-1)) !! (x-1)
    up
      | y-1 < 0        = Off
      | otherwise      = (cs !! (y-1)) !! x
    upRight
      | x+1 >= width-1 = Off
      | y-1 < 0        = Off
      | otherwise      = (cs !! (y-1)) !! (x+1)
    left
      | y-1 < 0        = Off
      | otherwise      = (cs !! y) !! (x-1)
    mid = (cs !! y) !! x
    right
      | y+1 >= width-1  = Off
      | otherwise       = (cs !! y) !! (x+1)
    downLeft
      | x-1 < 0         = Off
      | y+1 >= height-1 = Off
      | otherwise       = (cs !! (y+1)) !! (x-1)
    down
      | y+1 >= height-1 = Off
      | otherwise       = (cs !! (y+1)) !! x
    downRight
      | x+1 >= width-1  = Off
      | y+1 >= height-1 = Off
      | otherwise      = (cs !! (y+1)) !! (x+1)
    (width, height) = bounds w
neighborhood x y w = error "not implemented"

-- padFrame adds a border with off-state cells around the world.
padFrame :: World -> World
padFrame w@(Closed cs) = Closed (frame : map (\x -> [Off] ++ x ++ [Off]) cs ++ [frame])
  where
    frame = replicate newWidth Off
    newWidth = 2 + (fst . bounds $ w)
padFrame w = error "not implemented"

-- Cells returns the 2d-list representation of the cellular states.
cells :: World -> [[Cell]]
cells (Closed cs) = cs
cells w = error "not implemented"

-- randomWorld returns a world with random states for all cells.
randomWorld :: RandomGen g => g -> World -> World
randomWorld gen w@(Closed cs) = Closed $ splitEvery width . take (width*height) $ randoms gen
  where
    (width, height) = bounds w
randomWorld gen w = error "not implemented"

-- randomize sets the automatons cellular states to a random initilization.
randomize :: RandomGen g => Automaton -> g -> Automaton
randomize (Automaton l w) gen = Automaton l (randomWorld gen w)

-- toImg creates a vty image from an automaton world.
toImg :: Automaton -> Image
toImg (Automaton _ w) = vertCat . map colorize . cells $ w

-- toPic creates a vty picture from an automaton world.
toPic :: Automaton -> Picture
toPic (Automaton _ w) = picForImage . vertCat . map colorize . cells $ w

-- colorize assigns a color to each cell individually.
colorize :: [Cell] -> Image
colorize = horizCat . map cellColor

-- cellColor gives different colors for the different cell states.
cellColor :: Cell -> Image
cellColor c
  | c == On  = char (defAttr `withForeColor` magenta) character
  | c == Off = char (defAttr `withForeColor` brightBlack) character
  where
    character = head . show $ c

main :: IO ()
main = do
  let w = Closed (replicate height . replicate width $ Off)
  let game = Automaton life w

  vty <- standardIOConfig >>= mkVty

  graphicsLoop vty game
  shutdown vty

-- graphicsLoop updates the image and listens for user input for further actions.
graphicsLoop :: Vty -> Automaton -> IO ()
graphicsLoop vty a = do
  let buffer       = string (defAttr `withForeColor` green) ""
      instructions = string (defAttr `withForeColor` blue) " --- [ (Q) Quit | (A) Auto | (N) Next iteration | (R) Random cell states ] --- "
      game         = toImg a
      pic          = picForImage $ game <-> buffer <-> instructions

  update vty pic
  e <- nextEvent vty
  case e of
 -- Quit
    EvKey (KChar 'q') [] -> return ()
 -- Auto
    EvKey (KChar 'a') [] -> auto vty a
 -- Next iteration
    EvKey (KChar 'n') [] -> graphicsLoop vty (enforceLaw a)
 -- Randomize
    EvKey (KChar 'r') [] -> do
      gen <- newStdGen
      graphicsLoop vty (randomize a gen)
    _ -> graphicsLoop vty a

-- auto automatically plays the next iteration forever.
auto :: Vty -> Automaton -> IO ()
auto vty a = do
  let pic = toPic a
  update vty pic
  auto vty (enforceLaw a)
