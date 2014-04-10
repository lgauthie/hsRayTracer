module Main where

import Data.Packed.Vector
import Numeric.Container
import Codec.Picture
import Data.List (mapAccumR)

-- TODO: Is there a good way to actually constrain this to Vec3?
type Vec3  = Vector Double
type Color = Vec3

data Shape
    = Sphere !(Vec3) !Double
    | Plane  !(Vec3)
    deriving (Show)
data Ray = Ray
    { direction :: Vec3
    , origin :: Vec3
    } deriving (Show)
data World = World
    { entities :: [Entity]
    , lights :: [Vec3]
    , width :: Int
    , height :: Int
    , fov :: Double
    }
data Entity = Entity
    { shape :: Shape
    , color :: Color
    , shinyness :: Double
    }


aspectRatio :: World -> Double
aspectRatio world = w'/h'
  where w' = fromIntegral $ width world
        h' = fromIntegral $ height world

angle :: World -> Double
angle world = tan $ pi * 0.5 * fov world / 180.0

getColor :: Entity -> Color
getColor (Entity _ c _) = c

main :: IO ()
main = writePng "out.png" $ generateImage pixelRenderer w h
  where
    w = 640
    h = 480
    entities' =
        [ Entity {
            shape=Sphere (fromList [5.0, -1.0, -15.0]) 4
           ,color=fromList [1, 0, 0]
           ,shinyness=1.0
           }
        , Entity {
            shape=Sphere (fromList [0.0, -10003.0, -20.0]) 10000
           ,color=fromList [0, 1, 0]
           ,shinyness=24.0
           }
        , Entity {
            shape=Sphere (fromList [0.0, 0.0, -20.0]) 4
           ,color=fromList [0, 0, 1]
           ,shinyness=16.0
           }
        , Entity {
            shape=Sphere (fromList [5.0, 0.0, -25.0]) 4
           ,color=fromList [0, 1, 1]
           ,shinyness=8.0
           }
        , Entity {
            shape=Sphere (fromList [-5.5, 0.0, -25.0]) 4
           ,color=fromList [1, 1, 0]
           ,shinyness=8.0
           }
        ]
    world = World
        { entities = entities'
        , lights = [fromList [-16.0, 5.0, 15.0]]
        , width = w
        , height = h
        , fov = 70.0
        }
    pixelRenderer x y = toPixel $ traceRayThroughCoords x y world

toPixel :: Color -> PixelRGB8
toPixel vec = PixelRGB8 x y z
  where
    double2Word8 = fromIntegral . truncate
    x = double2Word8 $ (vec @> 0) * 128
    y = double2Word8 $ (vec @> 1) * 128
    z = double2Word8 $ (vec @> 2) * 128

normalize :: Vec3 -> Vec3
normalize v = scale ((/) 1 $ sqrt $ v `dot` v) v

traceRayThroughCoords :: Int -> Int -> World -> Color
traceRayThroughCoords x y world = traceRay world initialRay
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
    a = (2 * ((x' + 0.5) * inv_width) - 1) * angle world * aspectRatio world
    b = (1 - 2 * ((y' + 0.5) * inv_height)) * angle world
    dir = normalize $ 3 |> [a, b, -1.0]

traceRay :: World -> Ray -> Color
traceRay world ray = maybe (3 |> [0,0,0]) (computeLight world ray distance) entity
  where
    ((distance, entity), _) = mapAccumR fn (1/0, Nothing) (entities world)
    fn acc@(oldDist, oldEntity) entity' = case maybeDistance of
        Nothing      -> (acc, maybeDistance)
        Just newDist -> (if oldDist > newDist
                            then (newDist, Just entity')
                            else (oldDist, oldEntity) , maybeDistance)
      where maybeDistance = intersected ray $ shape entity'

computeLight :: World -> Ray -> Double -> Entity -> Color
computeLight world ray dist ent = specular_refl `add` diffuse_refl
  where
    -- light_dist = norm2 $ lights world !! 1 `sub` intersect
    intersect = origin ray `add` (dist `scale` direction ray)
    normal = getNormal intersect $ shape ent
    light_dir = normalize $ (head . lights) world `sub` intersect
    reflect_dir = reflect light_dir normal
    lambertian = max (light_dir `dot` normal) 0.0
    specular = if lambertian > 0.0
                  then max spec_angle 0.0 ** 5
                  else 0.0
      where spec_angle = reflect_dir `dot` direction ray
    specular_refl = specular `scale` (3 |> [1,1,1]) `mul` getColor ent
    diffuse_refl = lambertian `scale` (3 |> [0.9,0.9,0.9]) `mul` getColor ent

reflect :: Vec3 -> Vec3 -> Vec3
reflect i n =  i `sub` (2.0 `scale` (n `dot` i `scale` n))

getNormal :: Vec3  -- ^ The point of intersection
          -> Shape -- ^ The shape to get the normal for
          -> Vec3  -- ^ Returns the normal of the shape at the intersection
getNormal inter (Sphere center _) = normalize $ inter `sub` center

intersected :: Ray -> Shape -> Maybe Double
intersected ray (Sphere center radius)
    | tca < 0   = Nothing
    | d2 > r2   = Nothing
    | t0 > 0    = Just t0
    | otherwise = Just t1
  where
    l = center `sub` origin ray
    tca = l `dot` direction ray
    d2 = l `dot` l - tca * tca
    thc = sqrt $ r2 - d2
    t0 = tca - thc
    t1 = tca + thc
    r2 = radius * radius
