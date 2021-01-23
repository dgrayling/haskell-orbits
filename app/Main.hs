module Main where

import Lib

import Text.Printf (printf)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad (forM_, when)
import Control.Monad.Writer (WriterT, execWriterT, runWriterT, tell)

-- import Control.Monad.Trans (liftIO)

addList :: [Double] -> [Double] -> [Double]
addList (x:xs) (y:ys) = x+y : addList xs ys
addList _ _ = []

sumSquares :: [Double] -> [Double] -> Double
sumSquares (x:xs) (y:ys) = (x-y)^2 + sumSquares xs ys
sumSquares _ _ = 0

distance x y = sqrt (sumSquares x y)

data Point = Point 
  {
      s :: [Double]
     ,v :: [Double]
  }
  deriving (Read,Show)

pointDistance :: Point -> Point -> Double
pointDistance x y = distance (s x) (s y)

p1 = Point [0,0] [0,0]
p2 = Point [1,1] [0,0]
p3 = Point [-1,-1] [0,0]

p4 = Point [0,0] [1,0]

update :: Point -> (Point,Point)
update p = (p',p')
  where 
    p' = Point x y
      where x = addList (s p) (v p)
            y = v p

moveSeries :: State Point [Point]
moveSeries = do
  a1 <- state (update)
  a2 <- state (update)
  a3 <- state (update)
  return [a1,a2,a3]

moveWrite :: StateT Point (WriterT String IO) ()
moveWrite = do
  a1 <- state (update)
  _ <- (lift . tell) (printf "\n" ++ show a1)
  a1 <- state (update)
  _ <- (lift . tell) (printf "\n" ++ show a1)
  a1 <- state (update)
  _ <- (lift . tell) (printf "\n" ++ show a1)
  return ()

main :: IO ()
main = execWriterT (runStateT moveWrite p4) >>= putStrLn