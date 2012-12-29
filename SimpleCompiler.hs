import Core

import System.Environment (getArgs)
import System.Random (getStdGen)

main = do
  g <- getStdGen
  fName:_ <- getArgs
  s <- readFile fName
  output $ interpret g (map read $ words s)

output [] = return ()
output (row:rs) = outputRow row >> putStrLn "" >> output rs

outputRow [] = return ()
outputRow (x:xs) = putStr (show x ++ " ") >> outputRow xs
