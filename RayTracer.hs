module Main where

import Data.Packed.Vector
import Numeric.Container
import Codec.Picture
import Data.List (mapAccumR)

data Shape
    = Sphere !(Vector Double) !Double
    | Plane  !(Vector Double)
    deriving (Show)
data Ray = Ray
    { direction :: Vector Double
    , origin :: Vector Double
    } deriving (Show)
data World = World
    { entities :: [Entity]
    , width :: Int
    , height :: Int
    , fov :: Double
    }
data Entity = Entity
    { shape :: Shape
    , color :: Vector Double
    }


aspectRatio :: World -> Double
aspectRatio world = w'/h'
  where w' = fromIntegral $ width world
        h' = fromIntegral $ height world

angle :: World -> Double
angle world = tan $ pi * 0.5 * (fov world) / 180.0

getColor :: Entity -> Vector Double
getColor (Entity  _ c) = c

main :: IO ()
main = writePng "out.png" $ generateImage pixelRenderer w h
  where
    w = 300
    h = 240
    entities' =
        [ Entity {
            shape=Sphere (fromList [5.0, -1.0, -15.0]) 4
           ,color=fromList [1, 0, 0]
           }
        , Entity {
            shape=Sphere (fromList [0.0, -10003.0, -20.0]) 10000
           ,color=fromList [0, 1, 0]
           }
        , Entity {
            shape=Sphere (fromList [0.0, 0.0, -20.0]) 4
           ,color=fromList [0, 0, 1]
           }
        , Entity {
            shape=Sphere (fromList [5.0, 0.0, -25.0]) 4
           ,color=fromList [0, 1, 1]
           }
        , Entity {
            shape=Sphere (fromList [-5.5, 0.0, -25.0]) 4
           ,color=fromList [1, 1, 0]
           }
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
traceRay ray world = maybe (3 |> [0,0,0]) getColor entity
  where
    ((distance, entity), _) = mapAccumR fn (Nothing, Nothing) (entities world)
    fn acc entity = case maybeDistance of
        Nothing  -> (acc, maybeDistance)
        Just newDist -> case acc of
            (Nothing, _)              -> ((Just newDist, Just entity), maybeDistance)
            (Just oldDist, oldEntity) -> (if oldDist > newDist
                                            then (Just newDist, Just entity)
                                            else (Just oldDist, oldEntity), maybeDistance)
      where maybeDistance = intersected ray $ shape entity

--    reflect_dir = reflect(light_dir, normal)
--
--    lambertian = max(light_dir.dot(normal), 0.0);
--    specular = 0.0
--
--    if lambertian > 0.0:
--        spec_angle = reflect_dir.dot(ray_dir)
--        specular = pow(max(spec_angle, 0.0), entity.material.shininess)
--
--    specular_reflection = specular * entity.material.specular
--
--    diffuse_reflection = lambertian * entity.material.diffuse
--
--    m_color = entity.material.color * (1 - entity.material.transparency)
--    return specular_reflection * m_color \
--         + diffuse_reflection * m_color \
--         + entity.material.ambient * m_color


intersected :: Ray -> Shape -> Maybe Double
intersected ray (Sphere center radius)
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
