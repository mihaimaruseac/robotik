{-# LANGUAGE RecordWildCards #-}

module Core (interpret, Number) where

import Control.Arrow
import Control.Monad.State

import qualified Data.HashMap.Strict as M
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Debug.Trace

{- Types -}
type Number = Integer
type Position = Number
type Size = Number
type ID = Number
type Value = Number
type Direction = Number

data Board = B
  { robots :: V.Vector Robot
  , directives :: V.Vector Directive
  , dIx :: Int
  , xBoard :: M.HashMap Position (M.HashMap Position Value)
  , yBoard :: M.HashMap Position (M.HashMap Position Value)
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

type Triple = (Number, Number, Number)
type BoardState = State Board
type SimpleHit = Maybe (Position, Position)
type Hit = Maybe (Position, Position)

{-
Main function. The only one that is exported. Interprets the program and
gives back a list of list of all cells.
-}
interpret :: [Number] -> [[Number]]
interpret = undefined --V.toList . V.map V.toList . board . execState step . build

{- Stepping -}

{-
Executes all the steps in running the program.
-}
steps :: BoardState ()
steps = do
  directives <- gets directives
  dIx <- gets dIx
  when (dIx < V.length directives) $ doDirective >> steps

{-
Executes one robot directive.
-}
doDirective :: BoardState ()
doDirective = do
  directives <- gets directives
  dIx <- gets dIx
  let D{..} = directives V.! dIx
  r@R{..} <- extractRobot robotID >>= moveRobot direction
  updateBoard xPos yPos value
  s <- get
  put $ s
    { dIx = dIx + 1
    , robots = V.modify (\v -> VM.write v (fromInteger robotID) r) (robots s)
    }

{-
Updates the value on the board at a given position.
-}
updateBoard :: Position -> Position -> Value -> BoardState ()
updateBoard x y v = do
  xb <- gets xBoard
  yb <- gets yBoard
  s <- get
  put $ s { xBoard = update xb x y v, yBoard = update yb y x v }
  where
    update mp k1 k2 0 = case M.lookup k1 mp of
      Nothing -> mp
      Just mp' -> let nmp = M.delete k2 mp' in
        if M.null nmp
        then M.delete k1 mp
        else M.insert k1 nmp mp
    update mp k1 k2 v = case M.lookup k1 mp of
      Nothing -> M.insert k1 (M.singleton k2 v) mp
      Just mp' -> M.insert k1 (M.insert k2 v mp') mp

{-
Extracts a robot given it's ID.
-}
extractRobot :: ID -> BoardState Robot
extractRobot id = do
  robots <- gets robots
  return $ robots V.! (fromInteger id)

{-
Moves a robot in a direction. Returns the newly placed robot.
-}
moveRobot :: Direction -> Robot -> BoardState Robot
moveRobot dir r@(R{..}) = do
  hit <- getBlockingPosition dir r
  case hit of
    Nothing -> return r -- do nothing if robot couldn't be stopped
    Just (x, y) -> return $ r { xPos = x' x, yPos = y' y }
  where
    x' x = x - (1 - m) * f
    y' y = y - m * f
    f = 1 - 2 * div dir 2
    m = mod dir 2

{-
Returns the position where a robot moving in one direction will get stuck, or
Nothing if no such position exists.
-}
getBlockingPosition :: Direction -> Robot -> BoardState Hit
getBlockingPosition dir r@(R{..}) = do
  hitRobot <- getBlockingRobot dir r
  hitValue <- getBlockingValue dir r
  case hitRobot of
    Nothing -> case hitValue of
      Nothing -> return Nothing
      Just (x, y) -> return . Just $ (x, y)
    Just (rx, ry) -> case hitValue of
      Nothing -> return . Just $ (rx, ry)
      Just (vx, vy) -> case pointDistance (xPos, yPos) (rx, ry) (vx, vy) of
        GT -> return . Just $ (vx, vy)
        _ -> return . Just $ (rx, ry)

{-
Returns the position of the first blocking robot in one direction, if any, or
Nothing otherwise.
-}
getBlockingRobot :: Direction -> Robot -> BoardState SimpleHit
getBlockingRobot d r = do
  let (x, y) = (xPos r, yPos r)
  rs <- gets robots
  let rss = V.filter (filterRobot d x y) rs
  if V.null rss
  then return Nothing
  else return . Just $ closest (x, y) $ V.map (xPos &&& yPos) $ rss

{-
Returns the position of the first blocking cell in one direction, if any, or
Nothing otherwise.
-}
getBlockingValue :: Direction -> Robot -> BoardState SimpleHit
getBlockingValue d r
  | d `mod` 2 == 0 = do
    ypos <- gets yBoard
    case M.lookup y ypos of
      Nothing -> return Nothing
      Just xs -> case getValid (modulus r) (cmp d $ x) (M.toList xs) of
        [] -> return Nothing
        xs -> return . Just $ (L.minimumBy xClosest xs, y)
  | otherwise = do
    xpos <- gets xBoard
    case M.lookup x xpos of
      Nothing -> return Nothing
      Just ys -> case getValid (modulus r) (cmp d $ y) (M.toList ys) of
        [] -> return Nothing
        ys -> return . Just $ (x, L.minimumBy yClosest ys)
  where
    (x, y) = (xPos r, yPos r)
    xClosest a b = abs (a - x) `compare` abs (b - x)
    yClosest a b = abs (a - y) `compare` abs (b - y)
    cmp x = if x `div` 2 == 0 then (<) else (>)
    getValid m cmp = map fst . filter (both . (cmp *** rem m))
    rem m x = x `mod` m == 0
    both (True, True) = True
    both _ = False

{-
Filters the robot list depending on the direction and the current robot's
position.
-}
filterRobot :: Direction -> Position -> Position -> Robot -> Bool
filterRobot d x y r@(R{..})
  | d == 0 = xPos > x && yPos == y
  | d == 1 = yPos > y && xPos == x
  | d == 2 = xPos < x && yPos == y
  | d == 3 = yPos < y && xPos == x

{-
Returns the closest point to a reference from a vector of points.
-}
closest :: (Position, Position) -> V.Vector (Position, Position) -> (Position, Position)
closest (x, y) = V.minimumBy $ pointDistance (x, y)

{-
Computes an odering between two points, relative to an origin.
-}
pointDistance :: (Position, Position) -> (Position, Position) -> (Position, Position) -> Ordering
pointDistance (x, y) (a, b) (c, d) = t1 `compare` t2
  where
    t2 = abs (c - x) + abs (d - y)
    t1 = abs (a - x) + abs (b - y)

{- Building -}

{-
Builds the initial board, following the language syntax.
Extracts the description of robots and passes control to buildWithRobots.
-}
build :: [Number] -> Board
build [] = errorBoard
build (rs:nums)
  | rs > 0 = buildWithRobots rs r ns
  | otherwise = errorBoard
  where
    (r, ns) = first (map makeRobot) $ extractTriple rs nums []

{-
Builds the initial board, following the language syntax.
Extracts the directives and passes control to buildWithAllArgs to finish the
building step.
-}
buildWithRobots :: Number -> [Robot] -> [Number] -> Board
buildWithRobots _ _ [] = errorBoard -- need at least one directive
buildWithRobots rc rs ns = buildWithAllArgs rs is
  where
    is = map (makeDirective rc) $ extractTriple' ns []

{-
Finishes the building part of the initial board.
-}
buildWithAllArgs :: [Robot] -> [Directive] -> Board
buildWithAllArgs rs ds = B
  (V.fromList $ reverse rs)
  (V.fromList $ reverse ds)
  0
  M.empty
  M.empty

{- -- Auxiliaries -- -}

{-
Displays an error message if the definition is invalid.
-}
errorBoard :: a
errorBoard = error "Board definition was invalid. Recheck syntax."

{-
Builds a robot from a triple in the description language.
-}
makeRobot :: Triple -> Robot
makeRobot (m, px, py) = R m px py

{-
Builds a directive for one robot, ensuring that the references to robots and
directions are always in range.
-}
makeDirective :: Number -> Triple -> Directive
makeDirective rs (rid, dir, val) = D (trim rid rs) (trim dir 4) val

{-
Ensures that a value is between 0 and a maximum value.
-}
trim :: Number -> Number -> Number
trim px sx = let x = px `mod` sx in if x < 0 then x + sx else x

{-
Extracts a triple from the description language, raising an error if there are
not enough elements (as given by the first argument). Uses an accumulator as
the second argument.
-}
extractTriple :: Number -> [Number] -> [Triple] -> ([Triple], [Number])
extractTriple 0 ns ts = (ts, ns)
extractTriple n (a:b:c:ns) ts = extractTriple (n - 1) ns $ (a,b,c) : ts
extractTriple _ _ _ = errorBoard

{-
Extracts a triple from the description language, padding with 0-es the last
triple. Uses an accumulator as the second argument.
-}
extractTriple' :: [Number] -> [Triple] -> [Triple]
extractTriple' [] ts = ts
extractTriple' [a] ts = (a, 0, 0) : ts
extractTriple' [a, b] ts = (a, b, 0) : ts
extractTriple' (a:b:c:ns) ts = extractTriple' ns $ (a, b, c) : ts

{- Tests -}
hello1 = [3, 0, -1, 0, 0, 11, 0, 1, 0, 0,
  2, 0, 100,
  2, 2, 72,
  2, 0, 108,
  2, 2, 101,
  2, 0, 114,
  2, 2, 108,
  2, 0, 111,
  2, 2, 108,
  2, 0, 87,
  2, 2, 111,
  2, 0, 32]
hello2 = [3, 0, -1, 0, 0, 11, 0, 1, 0, 0,
  2, 0, 100,
  5, 2, 72,
  8, 4, 108,
  11, 6, 101,
  14, 8, 114,
  17, 10, 108,
  20, 12, 111,
  23, 14, 108,
  26, 16, 87,
  29, 18, 111,
  32, 20, 32]
hello3 = [3, 0, -1, 0, 0, 12, 0, 1, 0, 0,
  2, 1, 72,
  2, 0, 0,
  2, 2, 0,
  2, 1, 101,
  2, 0, 0,
  2, 2, 0,
  2, 1, 108,
  2, 0, 0,
  2, 2, 0,
  2, 1, 108,
  2, 0, 0,
  2, 2, 0,
  2, 1, 111,
  2, 0, 0,
  2, 2, 0,
  2, 1, 32,
  2, 0, 0,
  2, 2, 0,
  2, 1, 87,
  2, 0, 0,
  2, 2, 0,
  2, 1, 111,
  2, 0, 0,
  2, 2, 0,
  2, 1, 114,
  2, 0, 0,
  2, 2, 0,
  2, 1, 108,
  2, 0, 0,
  2, 2, 0,
  2, 1, 100]
hello4 = [3, 4242, -1, 0, 1, 1, 0, 0, 12, 0,
  0, 0, 72,
  1, 1, 1,
  1, 0, 0,
  1, 2, 0,
  0, 0, 101,
  1, 1, 1,
  1, 0, 0,
  1, 2, 0,
  0, 0, 108,
  1, 1, 1,
  1, 0, 0,
  1, 2, 0,
  0, 0, 108,
  1, 1, 1,
  1, 0, 0,
  1, 2, 0,
  0, 0, 111,
  1, 1, 1,
  1, 0, 0,
  1, 2, 0,
  0, 0, 32,
  1, 1, 1,
  1, 0, 0,
  1, 2, 0,
  0, 0, 87,
  1, 1, 1,
  1, 0, 0,
  1, 2, 0,
  0, 0, 111,
  1, 1, 1,
  1, 0, 0,
  1, 2, 0,
  0, 0, 114,
  1, 1, 1,
  1, 0, 0,
  1, 2, 0,
  0, 0, 108,
  1, 1, 1,
  1, 0, 0,
  1, 2, 0,
  0, 0, 100]
