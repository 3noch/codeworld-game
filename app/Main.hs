{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding ((.))
import qualified Data.Function as Func
import Control.Category ((<<<), (>>>))
import CodeWorld
import CodeWorld.Image
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe
import Control.Applicative
import Data.List (foldl')
import Data.Foldable

level1 =
  ["T     TTTT      TTTTTTTTTTTTTTTTTTTTTTTTTTT",
   "T  q   TT       TTTTTTTTTTTTTTTT      TTTTTT",
   "T       T       TTTTTTTTTTTTTT           TT",
   "T          P    TTTTTTTTTTTTTTT          TT",
   "T    P          TTTTTTTTTTTTTTTTTTTT    TTT",
   "T               TTTTTTTTTTTTTTTTT      TTTT",
   "WWWWWWWWWWWWW WWWWWWWWWWWWWWWWWWWWWW    WWW",
   "WWWWWWWWWWWWW WWWWWWWWWWWWWWWWWWWWW  WWWWWW",
   "WWWWWWWWWWWWW WWWWWWWWWWWWWWWWWWWW  WWWWWWW",
   "WWWWWWWWWWWWW WWWWWWWWWWWWWWWWWWW  WWWWW",
   "WWWWWWWWWW      WWWWWWWWWWWWWWW   WWWWWW",
   "WWWWWWWWWW      WWWWWWWWWWWWWWW WWWWWWWW",
   "WWWWWWWWWW  T                     WWWWWW",
   "WWWWWWWWWW      WWWWWWWWWWWWWWWWWWWWWWWW",
   "WWWWWWWWWWWLLLLLLLLLWWWWWWWWWWWWWWWWWWWW",
   "WWWWWWWWWWWLLLLLLLLLWWWWWWWWWWWWWWWWWWWW",
   "WWWWWWWWWLLLL     WWWWWWWWWWWWWWWWWWWWWW",
   "WWWWWWWWWWWWWWWW               WWWWWWWWWW",
   "WWWWWWWWWWWWWWWWWWWWWWWWWWWWWW    WWWWWWW",
   "WWWWWWWWWWWWWWWWWWWWWWWWWWWWW     WWWWWWWW",
   "WWWWWWWWWWWWWWWWWWWWWWWWWWW   q   WWWWWWW",
   "WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW",
   "WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW"]

level2 =
  ["T     TTTT      TTTTTTTTTTTTTTTTTTTTTTTTTTT",
   "T  q   TT       TTTTTTTTTTTTTTTT      TTTTTT",
   "T       T       TTTTTTTTTTTTTT           TT",
   "T          P    TTTTTTTTTTTTTTT          TT",
   "T    P          TTTTTTTTTTTTTTTTTTTT    TTT",
   "T               TTTTTTTTTTTTTTTTT      TTTT",
   "WWWWWWWWWWWWW WWWWWWWWWWWWWWWWWWWWWW    WWW",
   "WWWWWWWWWWWWW WWWWWWWWWWWWWWWWWWWWW  WWWWWW",
   "WWWWWWWWWWWWW WWWWWWWWWWWWWWWWWWWW  WWWWWWW",
   "WWWWWWWWWWWWW WWWWWWWWWWWWWWWWWWW  WWWWW",
   "WWWWWWWWWW      WWWWWWWWWWWWWWW   WWWWWW",
   "WWWWWWWWWW      WWWWWWWWWWWWWWW WWWWWWWW",
   "WWWWWWWWWW  T                     WWWWWW",
   "WWWWWWWWWW      WWWWWWWWWWWWWWWWWWWWWWWW",
   "WWWWWWWWWWWLLLLLLLLLWWWWWWWWWWWWWWWWWWWW",
   "WWWWWWWWWWWLLLLLLLLLWWWWWWWWWWWWWWWWWWWW",
   "WWWWWWWWWLLLL     WWWWWWWWWWWWWWWWWWWWWW",
   "WWWWWWWWWWWWWWWW               WWWWWWWWWW",
   "WWWWWWWWWWWWWWWWWWWWWWWWWWWWWW    WWWWWWW",
   "WWWWWWWWWWWWWWWWWWWWWWWWWWWWW     WWWWWWWW",
   "WWWWWWWWWWWWWWWWWWWWWWWWWWW   q   WWWWWWW",
   "WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW",
   "WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW"]

data Thing = Thing {
    thingPic :: Picture,
    thingWidth :: Double,
    thingHeight :: Double,
    thingOpaque :: Bool,
    thingCollisionRect :: Maybe (Rect Double),
    thingCollisionBehavior :: CollisionBehavior
  }

data CollisionBehavior
  = CollisionBlock
  | CollisionDamage Double
  | CollisionApply (Sprite -> Sprite)

data Sprite = Sprite {
    spriteThing :: Thing,
    spriteX :: Double,
    spriteY :: Double,
    spriteVelocityX :: Double,
    spriteVelocityY :: Double,
    spriteHealth :: Double,
    spriteMaxVelocity :: Double
  }

data World = World {
    worldPlayer :: Sprite,
    worldMap :: Map (Double, Double) LocationContent,
    worldDefaultTile :: Thing,
    worldDt :: Double
  }

data LocationContent = LocationContent {
    locationContentTile :: Maybe Thing, -- Opaque tile at this location
    locationContentThings :: [Thing]
  }

data Rect a = Rect { rectLeft :: a, rectTop :: a, rectRight :: a, rectBottom :: a }
rectCenterX (Rect l _ r _) = l + abs (l - r) / 2
rectCenterY (Rect _ t _ b) = t - abs (b - t) / 2

mapInsert :: Double -> Double -> Thing -> Map (Double, Double) LocationContent -> Map (Double, Double) LocationContent
mapInsert x y thing = Map.insertWith
  (\(LocationContent newTile newThings) (LocationContent oldTile oldThings) ->
    LocationContent (newTile <|> oldTile) (newThings ++ oldThings)
  )
  (y, x)
  (if thingOpaque thing then LocationContent (Just thing) [] else LocationContent Nothing [thing])

main = debugActivityOf world0 handle draw

world0 = World {
    worldPlayer = Sprite person 0 (-1) 0 0 5 4,
    worldMap = mkMap $ mapStr level1,
    worldDefaultTile = grass,
    worldDt = 0
  }

tree1 = (mkThing "Tree" "https://i.imgur.com/pTNgEtl.png" 2 3)
grass = (mkThing "Grass" "https://i.imgur.com/ySeUs5Z.png" 1.1 1.1) { thingOpaque = True, thingCollisionRect = Nothing }
water = (mkThing "Water" "https://i.imgur.com/sHorAql.png" 1.01 1.01) { thingOpaque = True }
lava = (mkThing "Lava" "https://i.imgur.com/gfy7sDy.png" 1.01 1.0) { thingOpaque = True, thingCollisionBehavior = CollisionApply $ \s -> s { spriteHealth = max 0 (spriteHealth s - 0.1) } }
squirtle = (mkThing "Squirtle" "https://i.imgur.com/MKoGXVw.png" 1 1) { thingCollisionBehavior = CollisionApply $ \s -> s { spriteHealth = min 5 (spriteHealth s + 0.1), spriteMaxVelocity = 10 } }
heartImg = image "Heart" "https://i.imgur.com/8g9MFyK.png" 1 1

person = (mkThing "Person" "https://i.imgur.com/cuGssCz.png" 1 1) { thingCollisionRect = Just (Rect 0.1 0 0.8 1) }

parseMapChar 'T' = Just tree1
parseMapChar 'W' = Just water
parseMapChar 'L' = Just lava
parseMapChar 'P' = Just person
parseMapChar 'q' = Just squirtle
parseMapChar _ = Nothing

mapStr str = map (map parseMapChar) str

mkMap :: [[Maybe Thing]] -> Map (Double, Double) LocationContent
mkMap rows = foldl' (\m (x, y, maybeThing) -> maybe m (\thing -> mapInsert (fromIntegral x) (fromIntegral y) thing m) maybeThing) Map.empty indexed
  where
    indexed = concat $
      for (height `div` 2) (-1) rows $ \(y, columns) ->
        for (-width `div` 2) 1 columns $ \(x, thing) -> (x, y, thing)
    for start step xs f = map f (zip [start, start+step..] xs)
    height = length rows
    width = maximum $ map length rows

mkThing :: Text -> Text -> Double -> Double -> Thing
mkThing name url w h = Thing (image name url w h) w h False (Just $ Rect 0 0 w h) CollisionBlock

calcMapSize m = max maxX maxY
  where
    ks = Map.keys m
    maxX = maximum $ map (abs <<< fst) ks
    maxY = maximum $ map (abs <<< snd) ks

draw world = frameRate & healthHearts & translated (-rectCenterX viewPort) (-rectCenterY viewPort) (drawRect viewPort & pictures things & pictures tiles) & solidRectangle 20 20
  where
    mapRect = Rect (-mapSize) mapSize mapSize (-mapSize)
    viewPort = limitViewPort mapRect (boundingRect (playerX, playerY) $ Rect 0 0 20 20)
    inViewPort (x, y) thing = overlapping viewPort $ boundingRect (x, y) $ Rect 0 0 (thingWidth thing) (thingHeight thing)
    mapSize = calcMapSize (worldMap world)
    tiles = [ translated x y $ thingPic thing
            | x <- [-mapSize..mapSize], y <- [mapSize, mapSize-1..(-mapSize)]
            , let thing = fromMaybe (worldDefaultTile world) $ locationContentTile =<< Map.lookup (y, x) theMap
            , inViewPort (x, y) thing
            ]
    things = [translated x y (thingPic thing) | ((y, x), loc) <- Map.toAscList theMap, thing <- locationContentThings loc, inViewPort (x, y) thing]

    theMap = mapInsert playerX playerY playerThing (worldMap world)
    Sprite playerThing playerX playerY _ _ playerHealth _ = worldPlayer world
    healthHearts = translated (-9.5) 9.5 (dilated 0.5 (heartArray 0 playerHealth))
    heartArray idx health | health <= 0 = blank
    heartArray idx health = translated (idx * 1.5) 0 heartImg & heartArray (idx+1) (health-1)
    
    frameRate = translated 9 9 $ lettering(T.pack $ show (round $ let x = worldDt world in if x == 0 then 0 else 1 / x))
    
    {-
    shownCollisionRects = viewPort : collisionRects' [(x, y, loc) | ((y, x), loc) <- Map.toAscList theMap]
    collisionRects' :: [(Double, Double, LocationContent)] -> [Rect Double]
    collisionRects' xs = [boundingRect (x, y) colRect | (x, y, LocationContent tile things) <- xs, thing <- maybeToList tile ++ things, Just colRect <- [collisionRect thing]]
    debug = lettering(T.pack $ show col) & pictures (map drawRect shownCollisionRects)
    col = collision (spriteCollisionRect $ worldPlayer world) [(x, y, loc) | ((y, x), loc) <- Map.toAscList $ worldMap world]
    -}

limitViewPort (Rect ml mt mr mb) (Rect l t r b) = Rect l' t' r' b'
  where
    w = r - l
    h = t - b
    (l', r') = if l < ml then (ml, ml + w) else if r > mr then (mr - w, mr) else (l, r)
    (t', b') = if t > mt then (mt, mt - h) else if b < mb then (mb + h, mb) else (t, b)

collision1 :: Rect Double -> Rect Double -> (Double, Double)
collision1 rect1@(Rect l0 t0 r0 b0) rect2@(Rect l1 t1 r1 b1) = if colliding then adjustment else (0, 0)
  where
    colliding = overlapping rect1 rect2
    leastChange = minimumBy (\(a, _) (b, _) -> abs a `compare` abs b)
      [(l1 - r0, False), (r1 - l0, False), (b1 - t0, True), (t1 - b0, True)]
    adjustment = case leastChange of
      (x, False) -> (x, 0)
      (y, True) -> (0, y)

overlapping (Rect l0 t0 r0 b0) (Rect l1 t1 r1 b1) = l0 < r1 && r0 > l1 && t0 > b1 && b0 < t1

boundingRect :: (Double, Double) -> Rect Double -> Rect Double
boundingRect (x, y) (Rect l t r b) = Rect l' t' (l' + w) (t' - h)
  where
    w = r - l
    h = b - t
    l' = x + l - w/2
    t' = y + t + h/2

buildCollisions :: Rect Double -> [(Double, Double, LocationContent)] -> (Double, Double, Sprite -> Sprite)
buildCollisions a m = foldl' doCollision (0, 0, id) $ collisionRects m
  where
    doCollision (x0, y0, f) (rect, behavior) = case behavior of
      CollisionApply eff | x1 /= 0 || y1 /= 0 -> (x0, y0, eff >>> f)
      CollisionBlock -> (biggestMag x0 x1, biggestMag y0 y1, f)
      _ -> (x0, y0, f)
      where
        (x1, y1) = collision1 a rect
    biggestMag a b = if abs a > abs b then a else b
    collisionRects :: [(Double, Double, LocationContent)] -> [(Rect Double, CollisionBehavior)]
    collisionRects xs = [(boundingRect (x, y) colRect, thingCollisionBehavior thing) | (x, y, LocationContent tile things) <- xs, thing <- maybeToList tile ++ things, Just colRect <- [thingCollisionRect thing]]

drawRect :: Rect Double -> Picture
drawRect (Rect l t r b) = polyline(points)
  where
    points = [(l, t), (r, t), (r, b), (l, b), (l, t)]

round' :: Double -> Double
round' x = fromIntegral (round (x * 10) :: Int) / 10

handle event = case event of
  KeyPress "Up" -> modPlayerVelocityY (+)
  KeyRelease "Up" -> modPlayerVelocityY (-)
  KeyPress "Down" -> modPlayerVelocityY (-)
  KeyRelease "Down" -> modPlayerVelocityY (+)
  KeyPress "Left" -> modPlayerVelocityX (-)
  KeyRelease "Left" -> modPlayerVelocityX (+)
  KeyPress "Right" -> modPlayerVelocityX (+)
  KeyRelease "Right" -> modPlayerVelocityX (-)
  TimePassing dt -> \world ->
    let
      (xBounce, yBounce, spriteCollisionEffect) = buildCollisions (spriteCollisionRect $ worldPlayer world) [(x, y, loc) | ((y, x), loc) <- Map.toAscList (worldMap world)]
      applyCollision = modPlayer spriteCollisionEffect <<< modPlayerLoc (+ xBounce) (+ yBounce)
    in
      applyCollision
    $ modPlayerLoc (+ ((world . worldPlayer . spriteVelocityX)) * dt) (+ ((world . worldPlayer . spriteVelocityY)) * dt)
    $ (world { worldDt = dt })
  _ -> id
  where
    modPlayerLoc fx fy = modPlayer (modSpriteY fy) <<< modPlayer (modSpriteX fx)
    modPlayerVelocityX f = modPlayer $ \s -> let v = spriteMaxVelocity s in modSpriteVelocityX (limited (-v) v <<< flip f v) s
    modPlayerVelocityY f = modPlayer $ \s -> let v = spriteMaxVelocity s in modSpriteVelocityY (limited (-v) v <<< flip f v) s
    limited low high x = min high $ max low x

spriteCollisionRect :: Sprite -> Rect Double
spriteCollisionRect (Sprite thing x y _ _ _ _) = boundingRect (x, y) $ fromMaybe (Rect 0 0 (thingWidth thing) (thingHeight thing)) (thingCollisionRect thing)

modPlayer f w = w { worldPlayer = f (worldPlayer w) }
modSpriteVelocityX f s = s { spriteVelocityX = f (spriteVelocityX s) }
modSpriteVelocityY f s = s { spriteVelocityY = f (spriteVelocityY s) }
modSpriteX f s = s { spriteX = f (spriteX s) }
modSpriteY f s = s { spriteY = f (spriteY s) }

infixl 1 .
(.) = (Func.&)
