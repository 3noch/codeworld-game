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
  ["                                           ",
   "                                           ",
   "                                           ",
   "                                           ",
   "T     TTTT      TTTTTTTTTTTTTTTTTTTTTTTTTTT",
   "T  q   TT       TTTTTTTTTTTTTTTT      TTTTTT",
   "T       T       TTTTTTTTTTTTTT           TT",
   "T          P    TTTTTTTTTTTTTTT       2  TT",
   "T               TTTTTTTTTTTTTTTTTTTT    TTT",
   "T   P           TTTTTTTTTTTTTTTTT      TTTT",
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
  ["LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLL                                        LLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLL                                        LLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLL          TTTTT                         LLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLL              T                         LLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLL           1  T                         LLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLL              T                         LLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLL        TTTTTTT                         LLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLL                                        LLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLL                                        LLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLL                                        LLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLL                                        LLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLL                                        LLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLL                     T                  LLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLL                                        LLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLL                                        LLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL"]

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
  | CollisionApply (World -> World)

data Sprite = Sprite {
    spriteThing :: Thing,
    spriteX :: Double,
    spriteY :: Double,
    spriteVelocityX :: Double,
    spriteVelocityY :: Double,
    spriteHealth :: Double,
    spriteMaxVelocity :: Double,
    spriteMovement :: MovementLogic
  }
data MovementLogic = MovementLogicUserControlled | MovementLogicStationary | MovementLogicGoToPlayer

data World = World {
    worldPlayer :: Sprite,
    worldMap :: Area,
    worldDefaultTile :: Thing,
    worldDt :: Double
  }

data Area = Area {
    areaTiles :: Map (Double, Double) LocationContent,
    areaSprites :: [Sprite],
    areaWidth :: Int,
    areaHeight :: Int
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

main = activityOf world0 handle draw

level1Area = mkArea $ areaStr level1
level2Area = mkArea $ areaStr level2

world0 = World {
    worldPlayer = Sprite person 0 (10) 0 0 5 4 MovementLogicUserControlled,
    worldMap = level1Area,
    worldDefaultTile = grass,
    worldDt = 0
  }

tree1 = (mkThing "Tree" "https://github.com/3noch/codeworld-game/raw/main/img/tree1.png" 2 3)
grass = (mkThing "Grass" "https://github.com/3noch/codeworld-game/raw/main/img/grass.png" 1.1 1.1) { thingOpaque = True, thingCollisionRect = Nothing }
water = (mkThing "Water" "https://github.com/3noch/codeworld-game/raw/main/img/water.png" 1.01 1.01) { thingOpaque = True }
lava = (mkThing "Lava" "https://github.com/3noch/codeworld-game/raw/main/img/lava.png" 1.01 1.0) { thingOpaque = True, thingCollisionBehavior = CollisionApply $ modPlayer $ \s -> s { spriteHealth = max 0 (spriteHealth s - 0.1) } }
squirtle = (mkThing "Squirtle" "https://github.com/3noch/codeworld-game/raw/main/img/squirtle.png" 1 1) { thingCollisionBehavior = CollisionApply $ modPlayer $ \s -> s { spriteHealth = min 5 (spriteHealth s + 0.1), spriteMaxVelocity = 10 } }
heartImg = image "Heart" "https://github.com/3noch/codeworld-game/raw/main/img/heart.png" 1 1

person = (mkThing "Person" "https://github.com/3noch/codeworld-game/raw/main/img/player.png" 1 1) { thingCollisionRect = Just (Rect 0.1 0 0.8 1) }

portalTo level = Thing (solidRectangle 1 1) 1 1 True (Just (Rect 0 0 1 1)) (CollisionApply $ \w -> modPlayerLoc (const 0) (const 0) $ (w { worldMap = level }))

parseMapChar 'T' = Just $ Right tree1
parseMapChar 'W' = Just $ Right water
parseMapChar 'L' = Just $ Right lava
parseMapChar 'P' = Just $ Left $ Sprite person 0 0 0 0 5 3 MovementLogicGoToPlayer
parseMapChar 'q' = Just $ Left $ Sprite squirtle 0 0 0 0 5 4 MovementLogicStationary
parseMapChar '2' = Just $ Right $ portalTo level2Area
parseMapChar '1' = Just $ Right $ portalTo level1Area
parseMapChar _ = Nothing

areaStr str = map (map parseMapChar) str

mkArea :: [[Maybe (Either Sprite Thing)]] -> Area
mkArea rows = foldl' (\m (x, y, maybeThing) -> maybe m (add m x y) maybeThing) (Area Map.empty [] width height) indexed
  where
    add m x y (Left sprite) = m { areaSprites = sprite { spriteX = x, spriteY = y } : areaSprites m }
    add m x y (Right thing) = m { areaTiles = mapInsert x y thing (areaTiles m) }
    indexed = concat $
      for (height `div` 2) (-1) rows $ \(y, columns) ->
        for (-width `div` 2) 1 columns $ \(x, thing) -> (fromIntegral x, fromIntegral y, thing)
    for start step xs f = map f (zip [start, start+step..] xs)
    height = length rows
    width = maximum $ map length rows

mkThing :: Text -> Text -> Double -> Double -> Thing
mkThing name url w h = Thing (image name url w h) w h False (Just $ Rect 0 0 w h) CollisionBlock

draw world = frameRate & healthHearts & translated (-rectCenterX viewPort) (-rectCenterY viewPort) (pictures things & pictures tiles) & solidRectangle 20 20
  where
    mapRect = boundingRect (0, 0) $ Rect 0 0 (fromIntegral $ areaWidth $ worldMap world) (fromIntegral $ areaHeight $ worldMap world)
    viewPort = limitViewPort mapRect (boundingRect (playerX, playerY) $ Rect 0 0 20 20)
    inViewPort (x, y) thing = overlapping viewPort $ boundingRect (x, y) $ Rect 0 0 (thingWidth thing) (thingHeight thing)
    tileViewPort = tileRect viewPort
    tiles = [ translated x y $ thingPic thing
            | x <- [rectLeft tileViewPort..rectRight tileViewPort], y <- [rectTop tileViewPort, rectTop tileViewPort-1..rectBottom tileViewPort]
            , let thing = fromMaybe (worldDefaultTile world) $ locationContentTile =<< Map.lookup (y, x) theMap
            , inViewPort (x, y) thing
            ]
    things = [translated x y (thingPic thing) | ((y, x), loc) <- Map.toAscList theMap, thing <- locationContentThings loc, inViewPort (x, y) thing]

    theMap = mapInsert playerX playerY playerThing $ mkAreaThings (worldMap world)
    Sprite playerThing playerX playerY _ _ playerHealth _ _ = worldPlayer world
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

mkAreaThings area = foldl' (\m sprite -> mapInsert (spriteX sprite) (spriteY sprite) (spriteThing sprite) m) (areaTiles area) (areaSprites area)

limitViewPort (Rect ml mt mr mb) (Rect l t r b) = Rect l' t' r' b'
  where
    w = r - l
    h = t - b
    (l', r') = if l < ml then (ml, ml + w) else if r > mr then (mr - w, mr) else (l, r)
    (t', b') = if t > mt then (mt, mt - h) else if b < mb then (mb + h, mb) else (t, b)

tileRect :: Rect Double -> Rect Double
tileRect (Rect l t r b) = Rect (fromIntegral $ floor l) (fromIntegral $ ceiling t) (fromIntegral $ ceiling r) (fromIntegral $ floor b)

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

buildCollisions :: Rect Double -> [(Double, Double, LocationContent)] -> (Double, Double, World -> World)
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
      getCollision sprite sprites = buildCollisions (spriteCollisionRect sprite) [(x, y, loc) | ((y, x), loc) <- Map.toAscList (mkAreaThings $ (worldMap world) { areaSprites = sprites })]
      doCollision sprites sprite =
        let (xBounce, yBounce, worldEffect) = getCollision sprite (filter (\x -> (spriteX x, spriteY x) /= (spriteX sprite, spriteY sprite)) sprites) in
        modSpriteX (+ xBounce) <<< modSpriteY (+ yBounce) $ sprite
      movedSprites = map (updateSpriteMovement dt (worldPlayer world)) $ areaSprites $ worldMap world
      collidedSprites = map (doCollision movedSprites) movedSprites
      
      movedPlayer = moveSprite dt $ worldPlayer world
      (xBounce, yBounce, worldEffect) = getCollision movedPlayer collidedSprites
      updateWorld w = w { worldDt = dt, worldMap = (worldMap world) { areaSprites = collidedSprites }, worldPlayer = movedPlayer }
      applyCollision = modPlayerLoc (+ xBounce) (+ yBounce) >>> worldEffect
    in
      updateWorld >>> applyCollision $ world
  _ -> id
  where
    modPlayerVelocityX f = modPlayer $ \s -> let v = spriteMaxVelocity s in modSpriteVelocityX (limited (-v) v <<< flip f v) s
    modPlayerVelocityY f = modPlayer $ \s -> let v = spriteMaxVelocity s in modSpriteVelocityY (limited (-v) v <<< flip f v) s
    limited low high x = min high $ max low x

updateSpriteMovement dt player sprite = case spriteMovement sprite of
  MovementLogicStationary -> sprite
  MovementLogicGoToPlayer -> moveSprite dt $ sprite { spriteVelocityX = spriteVelocityX, spriteVelocityY = spriteVelocityY }
    where
      spriteVelocityX = cos angleToPlayer * spriteMaxVelocity sprite
      spriteVelocityY = sin angleToPlayer * spriteMaxVelocity sprite
      vecDiff = vectorDifference (spriteX player, spriteY player) (spriteX sprite, spriteY sprite)
      angleToPlayer = vectorDirection vecDiff
  _ -> sprite

spriteCollisionRect :: Sprite -> Rect Double
spriteCollisionRect (Sprite thing x y _ _ _ _ _) = boundingRect (x, y) $ fromMaybe (Rect 0 0 (thingWidth thing) (thingHeight thing)) (thingCollisionRect thing)

modPlayer f w = w { worldPlayer = f (worldPlayer w) }
modPlayerLoc fx fy = modPlayer (modSpriteY fy) <<< modPlayer (modSpriteX fx)
modSpriteVelocityX f s = s { spriteVelocityX = f (spriteVelocityX s) }
modSpriteVelocityY f s = s { spriteVelocityY = f (spriteVelocityY s) }
modSpriteX f s = s { spriteX = f (spriteX s) }
modSpriteY f s = s { spriteY = f (spriteY s) }
moveSprite dt s = s { spriteX = spriteX s + spriteVelocityX s * dt, spriteY = spriteY s + spriteVelocityY s * dt }

infixl 1 .
(.) = (Func.&)
