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

delete i items = take (i-1) items ++ drop i items
eliminate items i = (delete i items,items !! (i-1))
recombine (x,xs) = x:xs

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
    , inertia :: Double
  }
  deriving (Read,Show)

pointDistance :: Point -> Point -> Double
pointDistance x y = distance (s x) (s y)

p1 = Point [0,0] [0,0]
p2 = Point [1,1] [0,0]
p3 = Point [-1,-1] [0,0]

p4 = Point [0,0] [1,0]

m1 = Mass (Point [0.0,0.0,0.0] [0.0,0.0,0.0]) 100.0 10000000000000.0
m2 = Mass (Point [2000.0,0.0,0.0] [0.0,4.0,0.0]) 1.0 100.0
m3 = Mass (Point [2.0,2.0,2.0] [0.0,0.0,0.0]) 1.0 1.0
m4 = Mass (Point [3.0,3.0,3.0] [-4.0,-2.0,1.0]) 1.0 1.0
m5 = Mass (Point [-1.0,-1.0,-2.0] [1.0,3.0,4.0]) 1.0 1.0
m6 = Mass (Point [1.0,0.0,-3.0] [1.0,-1.0,-1.5]) 1.0 1.0

updatePoint :: Point -> (Point,Point)
updatePoint p = (p',p')
  where 
    p' = Point x y
      where x = addList (s p) (v p)
            y = v p

updateMasses :: [Mass] -> ([Mass],[Mass])
updateMasses ms = (ms',ms')
  where 
    ms' = map f ms
      where f mass = Mass (Point (addList (s (p mass)) (v (p mass))) (v (p mass))) (m mass) (inertia mass)

acccelerateMassesOneState :: [Mass] -> ([Mass],[Mass])
acccelerateMassesOneState ms = (ms',ms')
  where ms' = accelerateMassesOne ms

massAcceleration :: Double -> Mass -> Mass -> [Double]
massAcceleration g x y = map (*magnitude) (displacement (s (p x)) (s (p y)))
  where 
    magnitude = (g*(m x)*(m y))/((s^2)*inertia x)
      where s = pointDistance (p x) (p y)

totalMassAcceleration :: Double -> Mass -> [Mass] -> [Double]
totalMassAcceleration g mass masses = foldl1 addList (map (massAcceleration g mass) masses)

accelerateMass :: [Double] -> Mass -> Mass
accelerateMass acceleration mass = mass'
  where mass' = Mass (Point (s (p mass)) (addList (v (p mass)) acceleration)) (m mass) (inertia mass)

accelerateMassFromGravForce g masses mass = mass'
  where mass' = accelerateMass (totalMassAcceleration g mass masses) mass 

accelerateMassFromGravForceTuple g (masses,mass) = accelerateMassFromGravForce g masses mass

generateInteractions masses = interactions
  where interactions = map (eliminate masses) [1..length masses] 

accelerateMasses :: Double -> [Mass] -> [Mass]
accelerateMasses g masses = masses'
  where masses' = map (accelerateMassFromGravForceTuple g) (generateInteractions masses)

accelerateMassesOne :: [Mass] -> [Mass]
accelerateMassesOne = accelerateMasses (-10)

moveSeries :: State Point [Point]
moveSeries = do
  a1 <- state (updatePoint)
  a2 <- state (updatePoint)
  a3 <- state (updatePoint)
  return [a1,a2,a3]

stuckWrite :: StateT Point (WriterT String IO) ()
stuckWrite = do
  a1 <- state (updatePoint)
  _ <- (lift . tell) (printf "\n" ++ show a1)
  return ()

moveWrite :: StateT Point IO ()
moveWrite = forever $ do
  a1 <- state (updatePoint)
  liftIO $ print a1

runLoop :: StateT [Mass] IO ()
runLoop = forM_ [1..100] $ \_ -> do
  _ <- state (updateMasses)
  _ <- state (acccelerateMassesOneState)
  return ()

simulateSystem :: StateT [Mass] IO ()
simulateSystem = forever $ do
  _ <- runLoop
  a1 <- state (updateMasses)
  a2 <- state (acccelerateMassesOneState)
  liftIO $ putStrLn . show $ (s $ p $ head a1) ++ (s $ p $ head $ tail a1)

-- movePair :: StateT [Mass] IO ()
-- movePair = forever $ do


-- main :: IO ()
main = runStateT simulateSystem [m1,m2] -- start loop at 10