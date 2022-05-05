{-# LANGUAGE OverloadedStrings, TupleSections #-}

import Control.Applicative
import Control.Category ((<<<), (>>>))
import CodeWorld
import CodeWorld.Image
import Data.Foldable
import Data.List (foldl', sortOn, insertBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T

main = activityOf world0 update draw

world0 = World {
    worldPlayer = Sprite person 0 10 0 0 5 4 MovementLogicStationary,
    worldAreas = Map.singleton "level1" level1,
    worldAreaName = "level1",
    worldDt = 0,
    worldNumStars = 0
  }

level1 = mkArea "level1" grass $ areaStr
  ["                                           ",
   "                                           ",
   "             3        ❤         🌟          ",
   "                                           ",
   "T     TTTT      TTTTTTTTTTTTTTTTTTTTTTTTTTT",
   "T  q   TT       TTTTTTTTTTTTTTTT      TTTTTT",
   "T       T       TTTTTTTTTTTTTT    🌟      TT",
   "T    b     x    TTTTTTTTTTTTTTT   ❤   2  TT",
   "T               TTTTTTTTTTTTTTTTTTTT    TTT",
   "T   ❤           TTTTTTTTTTTTTTTTT      TTTT",
   "WWWWWWWWWWWWW WWWWWWWWWWWWWWWWWWWWWW    WWW",
   "WWWWWWWWWWWWW WWWWWWWWWWWWWWWWWWWWW  WWWWWW",
   "WWWWWWWWWWWWW WWWWWWWWWWWWWWWWWWWW  WWWWWWW",
   "WWWWWWWWWWWWW WWWWWWWWWWWWWWWWWWW  WWWWW",
   "WWWWWWWWWW      WWWWWWWWWWWWWWW   WWWWWW",
   "WWWWWWWWWW      WWWWWWWWWWWWWWW WWWWWWWW",
   "WWWWWWWWWW  T        x            WWWWWW",
   "WWWWWWWWWW      WWWWWWWWWWWWWWWWWWWWWWWW",
   "WWWWWWWWWWW         WWWWWWWWWWWWWWWWWWWW",
   "WWWWWWWWWWW         WWWWWWWWWWWWWWWWWWWW",
   "WWWWWWWWW   b  ❤   WWWWWWWWWWWWWWWWWWWWWW",
   "WWWWWWWWWWWWWWWW               WWWWWWWWWW",
   "WWWWWWWWWWWWWWWWWWWWWWWWWWWWWW    WWWWWWW",
   "WWWWWWWWWWWWWWWWWWWWWWWWWWWWW     WWWWWWWW",
   "WWWWWWWWWWWWWWWWWWWWWWWWWWW   🌟  WWWWWWW",
   "WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW",
   "WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW"]

level2 = mkArea "level2" dirt $ areaStr
  ["LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLL                                        LLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLL                 ❤                      LLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLL          TTTTT                         LLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLL              T              ❤          LLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLL           1  T                         LLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLL              T                         LLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLL        TTTTTTT                         LLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLL                                        LLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLL                                        LLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLL                                        LLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLL       x                                LLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLL       x                                LLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLL       x             T                  LLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLL       x                                LLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLL                                        LLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL",
   "LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL"]

level3 = mkArea "level3" dirt $ areaStr
  [
   "ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg",
   "ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg",
   "ggggggggggggggggggggggg❤ggggggggggggggggggggggggggggggggggggggggggggggggggggg",
   "ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg",
   "ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg",
   "ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg",
   "                                TTTTTTTTT                                    ",
   "                                T T   ❤ T                                   ",
   "                                T T     T             ❤                     ",
   "                                T       T                                    ",
   "ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd",
   "LLLLLLdLLLLLLLLLLLLLLLLLLLLLLLLLLdLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLdLLLLLLLLLL",
   "LLLLLLdLLLLLLLLLLLLLLLLLLLLLLLLLLdLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLdLLLLLLLLLL",
   "LLLLLLdLLLLLLLLLLLLLLLLLLLLLLLLLLdLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLdLLLLLLLLLL",
   "ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd",
   "ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg",
   "ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg",
   "WWWWWWWWWWWWWWWWWWWWWWWWWggggggWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW",
   "WWWWWWWWWWWWWWWWWWWWWWWWWggggggWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW",
   "WWWWWWWWWWWWWWWWWWWWWWWWWggggggWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW",
   "WWWWWWWWWWWWWWWWWWWWWWWWWggggggWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW",
   "WWWWWWWWWWWWWWWWWWWWWWWWWggggggWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW",
   " 1                                                        ❤❤             2  "
  ]

tree1 = (mkThing "Tree" "https://github.com/3noch/codeworld-game/raw/main/img/tree1.png" 2 3) { thingCollisionRect = Just (Rect 0.4 (-0.8) 1.2 (-3)) }
grass = (mkThing "Grass" "https://github.com/3noch/codeworld-game/raw/main/img/grass.png" 1.1 1.1) { thingOpaque = True, thingCollisionRect = Nothing }
dirt = (mkThing "Dirt" "https://github.com/3noch/codeworld-game/raw/main/img/dirt.png" 1.1 1.1) { thingOpaque = True, thingCollisionRect = Nothing }
water = (mkThing "Water" "https://github.com/3noch/codeworld-game/raw/main/img/water.png" 1.1 1.1) { thingOpaque = True, thingCollisionRect = Just $ Rect 0 0 1 (-1) }
lava = (mkThing "Lava" "https://github.com/3noch/codeworld-game/raw/main/img/lava.png" 1.1 1.1) {
    thingCollisionRect = Just $ Rect 0 0 1 1,
    thingOpaque = True,
    thingCollisionBehavior = CollisionApply (applyToPlayer (\dt p -> p{ spriteHealth = max 0 (spriteHealth p - 5 * dt) })) False
  }
squirtle = (mkThing "Squirtle" "https://github.com/3noch/codeworld-game/raw/main/img/squirtle.png" 1 1) {
    thingCollisionBehavior = CollisionApply (applyToPlayer (\dt p -> p{ spriteHealth = min 8 (spriteHealth p + 2 * dt) })) False
  }
baltoy = (mkThing "Baltoy" "https://github.com/3noch/codeworld-game/raw/main/img/baltoy.png" 1 1) {
    thingCollisionBehavior = CollisionBlock
  }
heart = (mkThing "Heart" "https://github.com/3noch/codeworld-game/raw/main/img/heart.png" 0.5 0.5) {
    thingCollisionBehavior = CollisionApply (applyToPlayer (\dt p -> p{ spriteHealth = min 8 (spriteHealth p + 3) })) True
  }
baddy = (mkThing "Baddy" "https://github.com/3noch/codeworld-game/raw/main/img/lava.png" 1 1) {
    thingPic = solidCircle 0.5,
    thingCollisionBehavior = CollisionApply (applyToPlayer (\dt p -> p{ spriteHealth = max 0 (spriteHealth p - 2 * dt) })) False
  }
fright = (mkThing "Fright" "https://github.com/3noch/codeworld-game/raw/main/img/fright.png" 1 1) {
    thingPic = colored red (solidCircle 0.6),
    thingCollisionBehavior = CollisionApply (applyToPlayer (\dt p -> p{ spriteHealth = max 0 (spriteHealth p - 5 * dt)})) False
  }
mushroom = (mkThing "Mushroom" "https://github.com/3noch/codeworld-game/raw/main/img/mushroom.png" 0.5 0.5) {
    thingCollisionBehavior = CollisionApply (applyToPlayer (\dt p -> p{spriteMaxVelocity = spriteMaxVelocity p + 0.5 * dt})) True
  }
star = (mkThing "Start" "" 0.5 0.5) {
    thingPic = dilated 0.5 starPic,
    thingCollisionBehavior = CollisionApply (\dt w -> w{ worldNumStars = worldNumStars w + 1}) True
  }

starPic = lettering "🌟"
heartPic = image "Heart" "https://github.com/3noch/codeworld-game/raw/main/img/heart.png" 1 1

person = (mkThing "Person" "https://github.com/3noch/codeworld-game/raw/main/img/player.png" 1 1) { thingCollisionRect = Just (Rect 0.1 (-0.3) 0.8 (-0.7)) }

portalTo area x y = Thing (solidRectangle 1 1) 1 1 True (Just (Rect 0 0 1 1))
  (CollisionApply
    (\_ w -> modPlayerLoc (const x) (const y) $ w{
      worldAreaName = (areaName area), worldAreas = Map.insertWith (\new old -> old) (areaName area) area (worldAreas w)
    })
    False
  )

parseMapChar 'd' = Just $ Right dirt
parseMapChar 'D' = Just $ Right dirt
parseMapChar 'g' = Just $ Right grass
parseMapChar 'T' = Just $ Right tree1
parseMapChar 'W' = Just $ Right water
parseMapChar 'w' = Just $ Right water
parseMapChar 'L' = Just $ Right lava
parseMapChar 'P' = Just $ Left $ Sprite person 0 0 0 0 5 3 MovementLogicStationary
parseMapChar 'q' = Just $ Left $ Sprite squirtle 0 0 0 0 5 2 MovementLogicFleePlayer
parseMapChar 'b' = Just $ Left $ Sprite baltoy 0 0 0 0 5 4 MovementLogicFleePlayer
parseMapChar '❤' = Just $ Left $ Sprite heart 0 0 0 0 0 0 MovementLogicStationary
parseMapChar '🌟' = Just $ Left $ Sprite star 0 0 0 0 0 0 MovementLogicStationary
parseMapChar 'H' = Just $ Left $ Sprite heart 0 0 0 0 0 0 MovementLogicStationary
parseMapChar 'x' = Just $ Left $ Sprite baddy 0 0 0 0 5 3 MovementLogicGoToPlayer
parseMapChar 'F' = Just $ Left $ Sprite fright 0 0 0 0 5 2 MovementLogicGoToPlayer
parseMapChar '1' = Just $ Right $ portalTo level1 0 10
parseMapChar '2' = Just $ Right $ portalTo level2 0 0
parseMapChar '3' = Just $ Right $ portalTo level3 (-15) (-5)
parseMapChar _ = Nothing

draw world | worldNumStars world >= 5 = drawWin
draw world | spriteHealth (worldPlayer world) > 0 = drawAlive world
draw _ = drawDead

drawWin = lettering "You Won!" & translated 0 (-5) (dilated 0.5 (lettering "press Escape to restart")) & colored white (solidRectangle 40 40)
drawDead = colored white (lettering "Game Over" & translated 0 (-5) (dilated 0.5 (lettering "press Escape to restart"))) & solidRectangle 40 40
drawAlive world = frameRate & stars & healthHearts & translated (-rectCenterX viewPort) (-rectCenterY viewPort) (pictures things & pictures tiles) & solidRectangle 20 20
  where
    mapRect = boundingRect (0, 0) $ Rect 0 0 (fromIntegral $ areaWidth $ worldArea world) (fromIntegral $ areaHeight $ worldArea world)
    viewPort = limitViewPort mapRect (boundingRect (playerX, playerY) $ Rect 0 0 20 20)
    inViewPort (x, y) thing = isOverlapping viewPort $ boundingRect (x, y) $ Rect 0 0 (thingWidth thing) (thingHeight thing)
    tileViewPort = tileRect viewPort
    tiles = [ translated x y $ thingPic thing
            | x <- [rectLeft tileViewPort..rectRight tileViewPort], y <- [rectTop tileViewPort, rectTop tileViewPort-1..rectBottom tileViewPort]
            , let thing = fromMaybe (areaDefaultTile $ worldArea world) $ locationContentTile =<< Map.lookup (floor y, floor x) theMap
            , inViewPort (x, y) thing
            ]
    things = [translated x y (thingPic thing) | (_, loc) <- Map.toAscList theMap, ((y, x), thing) <- locationContentThings loc, inViewPort (x, y) thing]

    theMap = mapInsert playerX playerY playerThing $ mkAreaThings (worldArea world)
    Sprite playerThing playerX playerY _ _ playerHealth _ _ = worldPlayer world
    stars = translated (-9.5) 8.5 (dilated 0.5 (picArray starPic 0 (worldNumStars world)))
    healthHearts = translated (-9.5) 9.5 (dilated 0.5 (picArray heartPic 0 playerHealth))
    picArray pic idx amount | amount <= 0 = blank
    picArray pic idx amount = translated (idx * 1.5) 0 pic & picArray pic (idx+1) (amount-1)
    
    frameRate = translated 9 9 $ lettering(T.pack $ show (round $ let x = worldDt world in if x == 0 then 0 else 1 / x))

mapInsert :: Double -> Double -> Thing -> Map (Int, Int) LocationContent -> Map (Int, Int) LocationContent
mapInsert x y thing = Map.insertWith
  (\_ (LocationContent oldTile oldThings) ->
    if thingOpaque thing then LocationContent (newTile <|> oldTile) oldThings
                         else LocationContent oldTile (insertBy (comparing fst) newThing oldThings)
       
  )
  (floor y, floor x)
  (if thingOpaque thing then LocationContent newTile [] else LocationContent Nothing [newThing])
  where
    newTile = if thingOpaque thing then Just thing else Nothing
    newThing = ((y, x), thing)

mkAreaThings area = foldl' (\m sprite -> mapInsert (spriteX sprite) (spriteY sprite) (spriteThing sprite) m) (areaTiles area) (areaSprites area)

-- Types --
data Thing = Thing {
    thingPic :: Picture,
    thingWidth :: Double,
    thingHeight :: Double,
    thingOpaque :: Bool,
    thingCollisionRect :: Maybe (Rect Double),
    thingCollisionBehavior :: CollisionBehavior
  }
mkThing :: Text -> Text -> Double -> Double -> Thing
mkThing name url w h = Thing (image name url w h) w h False (Just $ Rect 0 0 w (-h)) CollisionBlock

data CollisionBehavior
  = CollisionBlock
  | CollisionApply (Double -> World -> World) Bool

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
data MovementLogic = MovementLogicStationary | MovementLogicGoToPlayer | MovementLogicFleePlayer

data World = World {
    worldPlayer :: Sprite,
    worldAreas :: Map String Area,
    worldAreaName :: String,
    worldDt :: Double,
    worldNumStars :: Int
  }
worldArea w = worldAreas w Map.! worldAreaName w

data Area = Area {
    areaName :: String,
    areaTiles :: Map (Int, Int) LocationContent,
    areaSprites :: [Sprite],
    areaWidth :: Int,
    areaHeight :: Int,
    areaDefaultTile :: Thing
  }
areaStr = map (map parseMapChar)

mkArea :: String -> Thing -> [[Maybe (Either Sprite Thing)]] -> Area
mkArea name defaultTile rows = foldl' (\m (x, y, maybeThing) -> maybe m (add m x y) maybeThing) (Area name Map.empty [] width height defaultTile) indexed
  where
    add m x y (Left sprite) = m { areaSprites = sprite { spriteX = x, spriteY = y } : areaSprites m }
    add m x y (Right thing) = m { areaTiles = mapInsert x y thing (areaTiles m) }
    indexed = concat $
      for (height `div` 2) (-1) rows $ \(y, columns) ->
        for (-width `div` 2) 1 columns $ \(x, thing) -> (fromIntegral x, fromIntegral y, thing)
    for start step xs f = zipWith (curry f) [start, start+step..] xs
    height = length rows
    width = maximum $ map length rows


data LocationContent = LocationContent {
    locationContentTile :: Maybe Thing, -- Opaque tile at this location
    locationContentThings :: [((Double, Double), Thing)] -- y, x, thing
  }

--   Rect
data Rect a = Rect { rectLeft :: a, rectTop :: a, rectRight :: a, rectBottom :: a } deriving Eq
rectCenterX (Rect l _ r _) = l + abs (l - r) / 2
rectCenterY (Rect _ t _ b) = t - abs (b - t) / 2
rectArea (Rect l t r b) = abs (r - l) * abs (b - t)
(.+) (x, y) (Rect l t r b) = Rect (l + x) (t + y) (r + x) (b + y)
isOverlapping (Rect l0 t0 r0 b0) (Rect l1 t1 r1 b1) = l0 < r1 && r0 > l1 && t0 > b1 && b0 < t1
rectOverlap a@(Rect l0 t0 r0 b0) b@(Rect l1 t1 r1 b1) = if isOverlapping a b then Just (Rect (max l0 l1) (min t0 t1) (min r0 r1) (max b0 b1)) else Nothing

limitViewPort (Rect ml mt mr mb) (Rect l t r b) = Rect l' t' r' b'
  where
    w = r - l
    h = t - b
    (l', r') = if l < ml then (ml, ml + w) else if r > mr then (mr - w, mr) else (l, r)
    (t', b') = if t > mt then (mt, mt - h) else if b < mb then (mb + h, mb) else (t, b)

tileRect :: Rect Double -> Rect Double
tileRect (Rect l t r b) = Rect (fromIntegral $ floor l) (fromIntegral $ ceiling t) (fromIntegral $ ceiling r) (fromIntegral $ floor b)

boundingRect :: (Double, Double) -> Rect Double -> Rect Double
boundingRect (x, y) (Rect l t r b) = Rect l' t' (l' + w) (t' - h)
  where
    w = abs (r - l)
    h = abs (b - t)
    l' = x + l - w/2
    t' = y + t + h/2

collision1 :: Rect Double -> Rect Double -> (Double, Double)
collision1 rect1@(Rect l0 t0 r0 b0) rect2@(Rect l1 t1 r1 b1) = if colliding then adjustment else (0, 0)
  where
    colliding = isOverlapping rect1 rect2
    leastChange = minimumBy (\(a, _) (b, _) -> abs a `compare` abs b)
      [(l1 - r0, False), (r1 - l0, False), (b1 - t0, True), (t1 - b0, True)]
    adjustment = case leastChange of
      (x, False) -> (x, 0)
      (y, True) -> (0, y)

buildCollisions :: Rect Double -> Map (Int, Int) LocationContent -> (Double, Double, Double -> World -> World)
buildCollisions r m = applyCollisionsTo r
  where
    foundCollisions = mapMaybe (\(rect, x) -> (, rect, x) <$> rectOverlap rect r) $ collisionRects nearbyThings
    sortedCollisions = sortOn (\(overlap, _, _) -> negate $ rectArea overlap) foundCollisions
    applyCollisionsTo rect = foldl' (doCollision rect) (0, 0, \_ w -> w) sortedCollisions
    doCollision rect0 (x, y, f) (_, rect1, behavior) = case behavior of
      CollisionApply eff removeSprite | x' /= 0 || y' /= 0 -> (x, y, \dt -> if removeSprite then eff dt >>> doRemoveSprite rect1 dt >>> f dt else eff dt >>> f dt)
      CollisionBlock -> (x + x', y + y', f)
      _ -> (x, y, f)
      where
        (x', y') = collision1 ((x, y) .+ rect0) rect1
    removeSpriteWithRect rect = filter (\sprite -> spriteCollisionRect sprite /= rect)

    doRemoveSprite rect _ = modArea $ \a -> a{areaSprites = removeSpriteWithRect rect (areaSprites a)}

    collisionRects :: [((Double, Double), Thing)] -> [(Rect Double, CollisionBehavior)]
    collisionRects things = [(boundingRect (x, y) colRect, thingCollisionBehavior thing) | ((y, x), thing) <- things, colRect <- maybeToList $ thingCollisionRect thing]

    (rectX, rectY) = (rectCenterX r, rectCenterY r)
    nearbyThings =
      [ thing
      | xOffset <- [-1, 0, 1]
      , yOffset <- [-1, 0, 1, 2]
      , let (x, y) = (floor $ rectX + xOffset, floor $ rectY + yOffset)
      , LocationContent tile things <- maybeToList $ Map.lookup (y, x) m
      , thing <- maybeToList (((fromIntegral y, fromIntegral x),) <$> tile) ++ things
      ]

update (KeyPress "Esc") w = world0
update event w | spriteHealth (worldPlayer w) > 0 = handle event w
update _ w = w

handle event = case event of
  KeyPress "Esc" -> \_ -> world0
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
      getCollision sprite sprites = buildCollisions (spriteCollisionRect sprite) $ mkAreaThings (worldArea world){areaSprites = sprites}
      doCollision sprites sprite =
        let (xBounce, yBounce, _) = getCollision sprite (filter (\x -> (spriteX x, spriteY x) /= (spriteX sprite, spriteY sprite)) sprites) in
        modSpriteX (+ xBounce) <<< modSpriteY (+ yBounce) $ sprite
      movedSprites = map (updateSpriteMovement dt (worldPlayer world)) $ areaSprites $ worldArea world
      collidedSprites = map (doCollision movedSprites) movedSprites

      movedPlayer = moveSprite dt $ worldPlayer world
      (xBounce, yBounce, worldEffect) = getCollision movedPlayer collidedSprites
      updateWorld w = modArea (\a -> a{areaSprites = collidedSprites}) $ w{worldDt = dt, worldPlayer = movedPlayer}
      applyCollision = modPlayerLoc (+ xBounce) (+ yBounce) >>> worldEffect dt
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
  MovementLogicFleePlayer -> if vecLength > 5 then sprite else moveSprite dt $ sprite { spriteVelocityX = -spriteVelocityX, spriteVelocityY = -spriteVelocityY }
  where
    spriteVelocityX = cos angleToPlayer * spriteMaxVelocity sprite
    spriteVelocityY = sin angleToPlayer * spriteMaxVelocity sprite
    vecDiff = vectorDifference (spriteX player, spriteY player) (spriteX sprite, spriteY sprite)
    vecLength = vectorLength vecDiff
    angleToPlayer = vectorDirection vecDiff

spriteCollisionRect (Sprite thing x y _ _ _ _ _) = boundingRect (x, y) $ fromMaybe (Rect 0 0 (thingWidth thing) (thingHeight thing)) (thingCollisionRect thing)

modPlayer f w = w { worldPlayer = f (worldPlayer w) }
modArea f w = w { worldAreas = Map.adjust f (worldAreaName w) (worldAreas w) }
modPlayerLoc fx fy = modPlayer (modSpriteY fy) <<< modPlayer (modSpriteX fx)
applyToPlayer f dt = modPlayer (f dt)
modSpriteVelocityX f s = s { spriteVelocityX = f (spriteVelocityX s) }
modSpriteVelocityY f s = s { spriteVelocityY = f (spriteVelocityY s) }
modSpriteX f s = s { spriteX = f (spriteX s) }
modSpriteY f s = s { spriteY = f (spriteY s) }
moveSprite dt s = s { spriteX = spriteX s + spriteVelocityX s * dt, spriteY = spriteY s + spriteVelocityY s * dt }
