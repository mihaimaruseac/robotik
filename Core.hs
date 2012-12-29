{-# LANGUAGE RecordWildCards #-}

module Core (interpret, Number) where

import Control.Arrow ((***), (&&&), first)
import Control.Monad.State (get, gets, put, State, execState, when)
import System.Random (StdGen, randomR)

import qualified Data.HashMap.Strict as M
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

{- Types -}
type Number = Integer
type Position = Number
type Size = Number
type ID = Number
type Value = Number
type Direction = Number

data Board = B
  { gen :: StdGen
  , robots :: V.Vector Robot
  , directives :: V.Vector Directive
  , dIx :: Int
  , xBoard :: M.HashMap Position (M.HashMap Position Value)
  , yBoard :: M.HashMap Position (M.HashMap Position Value)
  }

data Robot = R
  { modulus :: Value
  , xPos :: Position
  , yPos :: Position
  }

data Directive = D
  { robotID :: ID
  , direction :: Direction
  , value :: Value
  }

type Triple = (Number, Number, Number)
type BoardState = State Board
type Hit = Maybe (Position, Position)

{-
Main function. The only one that is exported. Interprets the program and
gives back a list of list of all cells.
-}
interpret :: StdGen -> [Number] -> [[Number]]
interpret g = extractArea . execState run . build g

extractArea :: Board -> [[Number]]
extractArea b = map outer [ymin .. ymax]
  where
    outer y = map (inner y) [xmin .. xmax]
    inner y x = let m = M.lookupDefault M.empty y vals in M.lookupDefault 0 x m
    vals = yBoard b
    (xmin, xmax) = getBoundsX b
    (ymin, ymax) = getBoundsY b

getBoundsX, getBoundsY :: Board -> (Position, Position)
getBoundsX B{robots=rs} = V.minimum &&& V.maximum $ V.map xPos rs
getBoundsY B{robots=rs} = V.minimum &&& V.maximum $ V.map yPos rs

{- Stepping -}

{-
Run driver.
-}
run :: BoardState ()
run = fixOverlaps >> get >>= \s -> put s { dIx = 0 } >> steps

{-
Executes all the steps in running the program.
-}
steps :: BoardState ()
steps = do
  directives <- gets directives
  dIx <- gets dIx
  when (dIx < V.length directives) $ doDirective >> steps

{-
Fixes overlaps between robots.
-}
fixOverlaps :: BoardState ()
fixOverlaps = do
  rbs <- gets robots
  dix <- gets dIx
  when (dix < V.length rbs) $ do
    let (x, rs) = V.splitAt (1 + dix) rbs
    let r = V.last x
    if r `overlaps` rs
    then (do
      g <- gets gen
      let (d, g') = randomR (0, 3) g
      let m = d `mod` 2
      let f = 1 - 2 * div d 2
      let x = xPos r - (1 - m) * f
      let y = yPos r - m * f
      s <- get
      put s
        { robots = V.modify (\v -> VM.write v dix $ r { xPos = x, yPos = y }) rbs
        , gen = g'
        , dIx = 0
        }
      fixOverlaps)
    else get >>= \s -> put s { dIx = dix + 1} >> fixOverlaps

{-
Tests whether one robots overlaps with others.
-}
overlaps :: Robot -> V.Vector Robot -> Bool
overlaps r rs = V.any (both . (((x ==) . xPos) &&& ((y ==) . yPos))) rs
  where
    both (True, True) = True
    both _ = False
    x = xPos r
    y = yPos r

{-
Executes one robot directive.
-}
doDirective :: BoardState ()
doDirective = do
  directives <- gets directives
  dix <- gets dIx
  let D{..} = directives V.! dix
  r@R{..} <- extractRobot robotID >>= moveRobot direction
  when (modulus /= 0) $ updateBoard xPos yPos value
  s <- get
  put $ s
    { dIx = dIx s + 1
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
Updates the directive index in case of a loop.
-}
updateLoopStart :: Number -> BoardState ()
updateLoopStart rid = do
  dIx <- gets dIx
  ds <- gets directives
  let v = fromInteger . value $ ds V.! dIx
  case filter (< dIx) $ V.toList . V.findIndices ((rid ==) . robotID) $ ds of
    [] -> return ()
    xs -> do
      let nIx = head . snd . splitAt (length xs - v) $ xs
      s <- get
      put s { dIx = nIx - 1 }

{-
Extracts a robot given it's ID.
-}
extractRobot :: ID -> BoardState Robot
extractRobot id = do
  robots <- gets robots
  return $ robots V.! fromInteger id

{-
Moves a robot in a direction. Returns the newly placed robot.
-}
moveRobot :: Direction -> Robot -> BoardState Robot
moveRobot dir r@(R{..}) = do
  hit <- getBlockingPosition dir r
  case hit of
    Nothing -> return r -- do nothing if robot couldn't be stopped
    Just (x, y) -> do
      moved <- if modulus == 0 then pushRobot x y dir else return Nothing
      case moved of
        Just ix -> do
          updateLoopStart ix
          return $ r { xPos = x, yPos = y }
        _ -> return $ r { xPos = x' x, yPos = y' y }
  where
    x' x = x - (1 - m) * f
    y' y = y - m * f
    f = 1 - 2 * div dir 2
    m = mod dir 2

{-
Pushes a robot. Returns True if push succeeded.
-}
pushRobot :: Position -> Position -> Direction -> BoardState (Maybe Number)
pushRobot x y d = do
  rToPush <- findRobotAt x y
  rToMiss <- findRobotAt x' y'
  case rToPush of
    Nothing -> return Nothing -- for completeness
    Just rP -> case rToMiss of
      Just _ -> return Nothing -- cannot push
      Nothing -> do
        s <- get
        rs <- gets robots
        let r' = rs V.! rP
        let r = r' { xPos = x', yPos = y' }
        put $ s { robots = V.modify (\v -> VM.write v rP r) rs }
        return . Just . toInteger $ rP
  where
    x' = x + (1 - m) * f
    y' = y + m * f
    f = 1 - 2 * div d 2
    m = mod d 2

{-
Returns the robot at a given position, if any or Nothing otherwise.
-}
findRobotAt :: Position -> Position -> BoardState (Maybe Int)
findRobotAt x y = do
  rs <- gets robots
  return $ V.findIndex (\R{..} -> x == xPos && y == yPos) rs

{-
Returns the position where a robot moving in one direction will get stuck, or
Nothing if no such position exists.
-}
getBlockingPosition :: Direction -> Robot -> BoardState Hit
getBlockingPosition dir r@(R{..}) = do
  hitRobot <- getBlockingRobot dir r
  hitValue <- if modulus /= 0 then getBlockingValue dir r else return Nothing
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
getBlockingRobot :: Direction -> Robot -> BoardState Hit
getBlockingRobot d r = do
  let (x, y) = (xPos r, yPos r)
  rs <- gets robots
  let rss = V.filter (filterRobot d x y) rs
  if V.null rss
  then return Nothing
  else return . Just . closest (x, y) . V.map (xPos &&& yPos) $ rss

{-
Returns the position of the first blocking cell in one direction, if any, or
Nothing otherwise.
-}
getBlockingValue :: Direction -> Robot -> BoardState Hit
getBlockingValue d r
  | d `mod` 2 == 0 = do
    ypos <- gets yBoard
    case M.lookup y ypos of
      Nothing -> return Nothing
      Just xs -> case getValid (modulus r) (cmp d x) (M.toList xs) of
        [] -> return Nothing
        xs -> return . Just $ (L.minimumBy xClosest xs, y)
  | otherwise = do
    xpos <- gets xBoard
    case M.lookup x xpos of
      Nothing -> return Nothing
      Just ys -> case getValid (modulus r) (cmp d y) (M.toList ys) of
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
build :: StdGen -> [Number] -> Board
build _ [] = errorBoard
build gen (rs:nums)
  | rs > 0 = buildWithRobots gen rs r ns
  | otherwise = errorBoard
  where
    (r, ns) = first (map makeRobot) $ extractTriple rs nums []

{-
Builds the initial board, following the language syntax.
Extracts the directives and passes control to buildWithAllArgs to finish the
building step.
-}
buildWithRobots :: StdGen -> Number -> [Robot] -> [Number] -> Board
buildWithRobots _ _ _ [] = errorBoard -- need at least one directive
buildWithRobots gen rc rs ns = buildWithAllArgs gen rs is
  where
    is = map (makeDirective rc) $ extractTriple' ns []

{-
Finishes the building part of the initial board.
-}
buildWithAllArgs :: StdGen -> [Robot] -> [Directive] -> Board
buildWithAllArgs gen rs ds = B gen
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
