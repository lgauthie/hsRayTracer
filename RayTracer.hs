module Main where

import Data.Packed.Vector
import Numeric.Container
import Codec.Picture

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
    , reflectiveness :: Double
    }

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
           ,reflectiveness=0
           }
        , Entity {
            shape=Sphere (fromList [0.0, -10003.0, -20.0]) 10000
           ,color=fromList [0.5, 0.5, 1]
           ,shinyness=24.0
           ,reflectiveness=0.9
           }
        , Entity {
            shape=Sphere (fromList [0.0, 0.0, -20.0]) 4
           ,color=fromList [0, 0, 1]
           ,shinyness=16.0
           ,reflectiveness=0
           }
        , Entity {
            shape=Sphere (fromList [5.0, 0.0, -25.0]) 4
           ,color=fromList [0, 1, 1]
           ,shinyness=8.0
           ,reflectiveness=0
           }
        , Entity {
            shape=Sphere (fromList [-5.5, 0.0, -25.0]) 4
           ,color=fromList [1, 1, 0]
           ,shinyness=8.0
           ,reflectiveness=0
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

-- | Helper to get world aspect ratio
aspectRatio :: World -> Double
aspectRatio world = w'/h'
  where w' = fromIntegral $ width world
        h' = fromIntegral $ height world

-- | Helper to get world angle
angle :: World -> Double
angle world = tan $ pi * 0.5 * fov world / 180.0

-- | Helper function to get an entities color
getColor :: Entity -> Color
getColor (Entity _ c _ _) = c

-- | Helper function to convert a color into a valid RGB value
toPixel :: Color -> PixelRGB8
toPixel vec = PixelRGB8 x y z
  where
    double2Word8 = fromIntegral . truncate
    x = double2Word8 $ (vec @> 0) * 128
    y = double2Word8 $ (vec @> 1) * 128
    z = double2Word8 $ (vec @> 2) * 128

-- | Calculates the color of a ray at the given coords
traceRayThroughCoords :: Int   -- ^ The pixel's x location
                      -> Int   -- ^ The pixel's y location
                      -> World -- ^ The world to render
                      -> Color -- ^ The color of the current pixel
traceRayThroughCoords x y world = traceRay world initialRay 5
  where initialRay = computeInitialRay x y world

-- | Helper to calculate the initil ray
-- | If I were to add a proper camera this would be the place to do it!
computeInitialRay :: Int   -- ^ The numper of pixels in the x dimension
                  -> Int   -- ^ The number of pixels in the y dimension
                  -> World -- ^ The world to be rendered
                  -> Ray   -- ^ Returns the initial ray to trace
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

traceRay :: World -- ^ The world to render
         -> Ray   -- ^ The current ray to trace
         -> Int   -- ^ The current ray to trace
         -> Color -- ^ The color of the current ray
traceRay world ray depth = maybe (3 |> [0,0,0]) recursivelyTrace entity
  where
    -- Find the nearest entity that intersects the current ray
    (distance, entity) = foldr fn (1/0, Nothing) (entities world)
    fn entity' acc@(oldDist, _) =
        case intersected ray $ shape entity' of
            Nothing      -> acc
            Just newDist -> if oldDist > newDist
                               then (newDist, Just entity')
                               else acc
    -- Helper to recursively trace reflective entities
    recursivelyTrace ent =
        if depth > 0 && refl > 0.05
           then curColor `add` traceRay world reflRay (depth - 1)
           else curColor
      where reflRay = Ray {origin=intersect', direction=reflDir}
            curColor = computeLight world ray intersect' ent
            reflDir = reflect (direction ray) $ getNormal intersect (shape ent)
            intersect' = intersect `add` (0.1 `scale` reflDir)
    refl = maybe 0 reflectiveness entity
    intersect = origin ray `add` (distance `scale` direction ray)

computeLight :: World  -- ^ The world to render
             -> Ray    -- ^ The current ray
             -> Vec3   -- ^ The intersection point
             -> Entity -- ^ The entity that is intersected
             -> Color  -- ^ Returns the color for the current ray/object/lights
computeLight world ray intersect ent = specular_refl `add` diffuse_refl
  where
    -- light_dist = norm2 $ lights world !! 1 `sub` intersect
    normal = getNormal intersect $ shape ent
    lightDir = normalize $ (head . lights) world `sub` intersect
    reflect_dir = reflect lightDir normal
    lambertian = max (lightDir `dot` normal) 0.0
    specular = if lambertian > 0.0
                  then max spec_angle 0.0 ** 5
                  else 0.0
      where spec_angle = reflect_dir `dot` direction ray
    specular_refl = specular `scale` (3 |> [1,1,1]) `mul` getColor ent
    diffuse_refl = lambertian `scale` (3 |> [0.9,0.9,0.9]) `mul` getColor ent

-- | Used to reflect a vector about a given normal
reflect :: Vec3 -- ^ The incoming Vector
        -> Vec3 -- ^ The normal to reflect around
        -> Vec3 -- ^ Returns the reflected Vector
reflect i n =  i `sub` (2.0 `scale` (n `dot` i `scale` n))

refract :: Vec3 -> Vec3 -> Double -> Double -> Maybe Vec3
refract i n refIndex1 refIndex2
    | sinT2 > 1 = Nothing
    | otherwise = Just val
  where
    ref = refIndex1/refIndex2
    n' = (-1) `scale` n
    cosI = n' `dot` i
    sinT2 = ref * ref * (1 - cosI - cosI)
    cosT = sqrt (1 - sinT2)
    val = (ref `scale` i) `add` ((ref * cosI - cosT) `scale` n)


-- def refract(I, N, n1, n2):
--     """
--     n1 -- index of refraction of original medium
--     n2 -- index of refraction of new medium
--     """
--     n = n1/n2
--     cosI = -N.dot(I)
--     sinT2 = n * n * (1 - cosI * cosI)
--     if sinT2 > 1:
--         return None
--     cosT = m.sqrt(1 - sinT2)
--     return n * I + (n * cosI - cosT) * N


getNormal :: Vec3  -- ^ The point of intersection
          -> Shape -- ^ The shape to get the normal for
          -> Vec3  -- ^ Returns the normal of the shape at the intersection
getNormal inter (Sphere center _) = normalize $ inter `sub` center

-- | Normalizes a given vector
normalize :: Vec3 -> Vec3
normalize v = scale ((/) 1 $ sqrt $ v `dot` v) v

intersected :: Ray          -- ^ The ray to check against the shape
            -> Shape        -- ^ The shape to check for collision
            -> Maybe Double -- ^ Returns Maybe distance
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
