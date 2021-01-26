module Main where

import Lib

import Text.Printf (printf)
import Control.Monad
-- import Control.Monad.State (get)
-- import Control.Monad.Loop as MLoop
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

displacement (x:xs) (y:ys) = x-y : displacement xs ys
displacement _ _ = []



data Point = Point 
  {
      s :: [Double]
     ,v :: [Double]
  }
  deriving (Read,Show)

data Mass = Mass 
  {
      p :: Point
    , m :: Double
  }
  deriving (Read,Show)

pointDistance :: Point -> Point -> Double
pointDistance x y = distance (s x) (s y)

p1 = Point [0,0] [0,0]
p2 = Point [1,1] [0,0]
p3 = Point [-1,-1] [0,0]

p4 = Point [0,0] [1,0]

m1 = Mass (Point [0.0,0.0] [1.0]) 1.0
m2 = Mass (Point [1.0,1.0] [1.0]) 1.0
m3 = Mass (Point [2.0,2.0] [1.0]) 1.0
m4 = Mass (Point [3.0,3.0] [1.0]) 1.0
m5 = Mass (Point [-1.0,-1.0] [1.0]) 1.0
m6 = Mass (Point [1.0,0.0] [1.0]) 1.0

update :: Point -> (Point,Point)
update p = (p',p')
  where 
    p' = Point x y
      where x = addList (s p) (v p)
            y = v p

massForce :: Double -> Mass -> Mass -> [Double]
massForce g x y = map (*magnitude) (displacement (s (p x)) (s (p y)))
  where 
    magnitude = (g*(m x)*(m y))/(s^2)
      where s = pointDistance (p x) (p y)

totalMassForce :: Double -> Mass -> [Mass] -> [Double]
totalMassForce g mass masses = foldl1 addList (map (massForce g mass) masses)

-- accelerateMasses :: [Mass] -> [Mass]
-- accelerateMasses 

-- massAccelerate :: [Mass] -> ([Mass],[Mass])
-- massAccelerate masses = (masses', masses)
--   where
--     masses' = 

moveSeries :: State Point [Point]
moveSeries = do
  a1 <- state (update)
  a2 <- state (update)
  a3 <- state (update)
  return [a1,a2,a3]

stuckWrite :: StateT Point (WriterT String IO) ()
stuckWrite = do
  a1 <- state (update)
  _ <- (lift . tell) (printf "\n" ++ show a1)
  return ()

moveWrite :: StateT Point IO ()
moveWrite = forever $ do
  a1 <- state (update)
  liftIO $ print a1

-- movePair :: StateT [Mass] IO ()
-- movePair = forever $ do


-- main :: IO ()
main = runStateT moveWrite p4 -- start loop at 10