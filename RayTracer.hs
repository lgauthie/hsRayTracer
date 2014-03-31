module Main where

import Data.Packed.Vector
import Numeric.Container
import Codec.Picture
import Data.List (mapAccumL)

data Shape
    = Sphere !(Vector Double) !Double !(Vector Double)
    | Plane  !(Vector Double)
    deriving (Show)
data Ray = Ray
    { direction :: Vector Double
    , origin :: Vector Double
    } deriving (Show)
data World = World
    { entities :: [Shape]
    , width :: Int
    , height :: Int
    , fov :: Double
    } deriving (Show)

aspectRatio :: World -> Double
aspectRatio world = w'/h'
  where w' = fromIntegral $ width world
        h' = fromIntegral $ height world

angle :: World -> Double
angle world = tan $ pi * 0.5 * (fov world) / 180.0

color :: Shape -> Vector Double
color (Sphere _ _ c) = c

main :: IO ()
main = writePng "out.png" $ generateImage pixelRenderer w h
  where
    w = 300
    h = 240
    entities' =
        [ Sphere (fromList [5.0, -1.0, -15.0]) 4 (fromList [128, 0, 0])
        , Sphere (fromList [0.0, -10003.0, -20.0]) 10000 (fromList [0, 128, 0])
        , Sphere (fromList [0.0, 0.0, -20.0]) 4 (fromList [0, 0, 128])
        , Sphere (fromList [5.0, 0.0, -25.0]) 4 (fromList [0, 128, 128])
        , Sphere (fromList [-5.5, 0.0, -25.0]) 4 (fromList [128, 128, 0])
        ]
    world = World
        { entities = entities'
        , width = w
        , height = h
        , fov = 70.0
        }
    pixelRenderer x y = toPixel $ traceRayThroughCoords x y world

toPixel :: Vector Double -> PixelRGB8
toPixel vec = PixelRGB8 x y z
  where
    double2Word8 = fromIntegral . truncate
    x = double2Word8 $ vec @> 0
    y = double2Word8 $ vec @> 1
    z = double2Word8 $ vec @> 2

normalize :: Vector Double -> Vector Double
normalize v = scale ((/) 1 $ sqrt $ v `dot` v) v

traceRayThroughCoords :: Int -> Int -> World -> Vector Double
traceRayThroughCoords x y world = traceRay initialRay world
  where
    initialRay = computeInitialRay x y world

computeInitialRay :: Int -> Int -> World -> Ray
computeInitialRay x y world = Ray { direction = dir, origin = 3 |> [0,0,0] }
  where
    x' = fromIntegral x :: Double
    y' = fromIntegral y :: Double
    h' = fromIntegral $ height world :: Double
    w' = fromIntegral $ width world :: Double
    inv_width  = 1/w'
    inv_height = 1/h'
    a = (2 * ((x' + 0.5) * inv_width) - 1) * (angle world) * (aspectRatio world)
    b = (1 - 2 * ((y' + 0.5) * inv_height)) * (angle world)
    dir = normalize $ 3 |> [a, b, -1.0]

traceRay :: Ray -> World -> Vector Double
traceRay ray world = c
  where
    -- Oh dear god...
    ((intersect, c), _) = mapAccumL fn (Nothing, 3 |> [0,0,0]) (entities world)
    fn acc x = case intersection of
        Nothing -> (acc, intersection)
        Just new -> case acc of
            (Nothing, _) -> ((Just new, color x), intersection)
            (Just old, c) -> (if old > new then (Just new, color x) else (Just old, c), intersection)
      where intersection = intersected ray x

intersected :: Ray -> Shape -> Maybe Double
intersected ray s@(Sphere center radius _)
    | tca < 0       = Nothing
    | d2 > radius^2 = Nothing
    | t0 > 0        = Just t0
    | otherwise     = Just t1
  where
    l = center `sub` origin ray
    tca = l `dot` direction ray
    d2 = l `dot` l - tca * tca
    thc = sqrt $ radius^2 - d2
    t0 = tca - thc
    t1 = tca + thc
