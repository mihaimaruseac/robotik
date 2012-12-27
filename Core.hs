module Core where

import Control.Arrow

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

type Number = Integer
type Position = Number
type Size = Number
type ID = Number
type Value = Number
type Direction = Number

data Board = B
  { xSize :: Size
  , ySize :: Size
  , robots :: V.Vector Robot
  , directives :: [Directive]
  , board :: V.Vector (V.Vector Value)
  } deriving (Eq, Show)

data Robot = R
  { modulus :: Number
  , xPos :: Position
  , yPos :: Position
  } deriving (Eq, Show)

data Directive = D
  { robotID :: ID
  , direction :: Direction
  , value :: Value
  } deriving (Eq, Show)

{- Tests -}
test1 = [1, 11, 1, 1, 0, 0, 11, 0, 1, 100, 0, 3, 72, 0, 1, 108, 0, 3, 101, 0,
  1, 114, 0, 3, 108, 0, 1, 111, 0, 3, 108, 0, 1, 87, 0, 3, 111, 0, 1, 32]
test2 = [1, 12, 1, 0, 0, 0, 11, 0, 1, 72, 0, 1, 101, 0, 1, 108, 0, 1, 108, 0,
  1, 111, 0, 1, 32, 0, 1, 87, 0, 1, 111, 0, 1, 114, 0, 1, 108, 0, 1, 100]
test3 = [2, 11, 2, 37, 0, 9, 2, 1, 2, 13, 0, 1, 100, 1, 2, 108, 1, 1, 108, 1,
  3, 108, 1, 1, 114, 1, 3, 111, 1, 1, 111, 1, 0, 0, 0, 3, 32, 1, 3, 0, 0, 1,
  87, 1, 2, 72, 1, 1, 101]

build :: [Number] -> Board
build (x:y:nums)
  | x > 0 && y > 0 = buildWithSize x y nums
  | otherwise = errorBoard
build _ = errorBoard

buildWithSize :: Size -> Size -> [Number] -> Board
buildWithSize _ _ [] = errorBoard
buildWithSize x y (rs:nums)
  | rs > 0 = buildWithSizeAndRobots x y rs r ns
  | otherwise = errorBoard
  where
    (r, ns) = first (map $ makeRobot x y) $ extractTriple rs nums []

buildWithSizeAndRobots :: Size -> Size -> Number -> [Robot] -> [Number] -> Board
buildWithSizeAndRobots _ _ _ _ [] = errorBoard
buildWithSizeAndRobots x y rc rs (ds:nums)
  | ds > 0 = buildWithAllArgs x y rs is
  | otherwise = errorBoard
  where
    (is, _) = first (map $ makeDirective rc) $ extractTriple ds nums []

buildWithAllArgs :: Size -> Size -> [Robot] -> [Directive] -> Board
buildWithAllArgs x y rs ds = B x y (V.fromList $ reverse rs) (reverse ds) (emptyBoard x y)

emptyBoard :: Size -> Size -> V.Vector (V.Vector Value)
emptyBoard x y = V.replicate (fromInteger x) (V.replicate (fromInteger y) 0)

errorBoard :: a
errorBoard = error "Board definition was invalid. Recheck syntax."

type Triple = (Number, Number, Number)

makeRobot :: Size -> Size -> Triple -> Robot
makeRobot sx sy (m, px, py) = R m (trim px sx) (trim py sy)

makeDirective :: Number -> Triple -> Directive
makeDirective rs (rid, dir, val) = D (trim rid rs) (trim dir 4) val

trim :: Number -> Number -> Number
trim px sx = let x = px `mod` sx in if x < 0 then x + sx else x

extractTriple :: Number -> [Number] -> [Triple] -> ([Triple], [Number])
extractTriple 0 ns ts = (ts, ns)
extractTriple n (a:b:c:ns) ts = extractTriple (n - 1) ns $ (a,b,c) : ts
extractTriple _ _ _ = errorBoard

