{-# LANGUAGE RecordWildCards #-}

module Core where

import Control.Arrow
import Control.Monad.State

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

{- Types -}
type Number = Int
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
  { modulus :: Value
  , xPos :: Position
  , yPos :: Position
  } deriving (Eq, Show)

data Directive = D
  { robotID :: ID
  , direction :: Direction
  , value :: Value
  } deriving (Eq, Show)

type BoardState = State Board

{- Stepping -}
step :: BoardState ()
step = do
  directives <- gets directives
  when (directives /= []) $ firstDirective directives >> step

firstDirective :: [Directive] -> BoardState ()
firstDirective (D{..}:ds) = do
  r@R{..} <- extractRobot robotID >>= moveRobot direction value
  s <- get
  put $ s
    { board = modify2DPos xPos yPos value (board s)
    , directives = ds
    , robots = V.modify (\v -> VM.write v robotID r) (robots s)
    }

extractRobot :: ID -> BoardState Robot
extractRobot id = do
  robots <- gets robots
  return $ robots V.! id

moveRobot :: Direction -> Value -> Robot -> BoardState Robot
moveRobot dir val r@(R{..}) = do
  brd <- gets board
  xs <- gets xSize
  ys <- gets ySize
  let (x, y) = getNextPos brd xs ys dir modulus xPos yPos
  return $ r { xPos = x, yPos = y }

getNextPos :: V.Vector (V.Vector Value) -> Size -> Size -> Direction -> Value
  -> Position -> Position -> (Position, Position)
getNextPos brd sx sy dir modulus x y
  | cx < 0 = (x, y)
  | cy < 0 = (x, y)
  | cx == sx = (x, y)
  | cy == sy = (x, y)
  | modulus == 0 = (cx, cy)
  | v /= 0 && v `mod` modulus == 0 = (x, y)
  | otherwise = getNextPos brd sx sy dir modulus cx cy
  where
    (cx, cy) = getNeighbour dir x y
    v = brd V.! cx V.! cy

getNeighbour :: Direction -> Position -> Position -> (Position, Position)
getNeighbour 0 x y = (x + 1, y)
getNeighbour 1 x y = (x, y + 1)
getNeighbour 2 x y = (x - 1, y)
getNeighbour 3 x y = (x, y - 1)

modify2DPos :: Position -> Position -> Value -> V.Vector (V.Vector Value) -> V.Vector (V.Vector Value)
modify2DPos x y v brd = V.modify outer brd
  where
    outer arg = VM.write arg x $ V.modify inner $ brd V.! x
    inner arg = VM.write arg y v

{- Building -}
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
emptyBoard x y = V.replicate x (V.replicate y 0)

{- Auxiliaries -}
type Triple = (Number, Number, Number)

errorBoard :: a
errorBoard = error "Board definition was invalid. Recheck syntax."

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

{- Tests -}
test1 = [1, 11, 1, 1, 0, 0, 11, 0, 1, 100, 0, 3, 72, 0, 1, 108, 0, 3, 101, 0,
  1, 114, 0, 3, 108, 0, 1, 111, 0, 3, 108, 0, 1, 87, 0, 3, 111, 0, 1, 32]
test2 = [1, 12, 1, 0, 0, 0, 11, 0, 1, 72, 0, 1, 101, 0, 1, 108, 0, 1, 108, 0,
  1, 111, 0, 1, 32, 0, 1, 87, 0, 1, 111, 0, 1, 114, 0, 1, 108, 0, 1, 100]
test3 = [2, 11, 3, 37, 0, 9, 2, 1, 2, 42, 1, 7, 13, 0, 1, 100, 1, 2, 108,
  1, 1, 108, 1, 3, 108, 1, 1, 114, 1, 3, 111, 1, 0, 0, 0, 3, 32, 2, 2, 111,
  1, 3, 0, 0, 1, 87, 1, 2, 72, 1, 1, 101]
board1 = build test1
board2 = build test2
board3 = build test3
board4 = B {xSize = 2, ySize = 11, robots = V.fromList [R {modulus = 37, xPos
  = 0, yPos = 9}], directives = [], board = V.fromList [V.fromList
  [0,0,0,0,0,0,0,0,0,0,0],V.fromList [0,0,0,0,0,0,0,0,0,0,0]]}
