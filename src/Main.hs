{-# LANGUAGE TemplateHaskell #-}

module Main(main) where

import           Control.Lens
import           Data.Maybe                         (catMaybes)
import           Graphics.Gloss.Data.Color          (Color (..), makeColor, white)
import           Graphics.Gloss.Data.Display        (Display (..))
import           Graphics.Gloss.Data.Picture        (Picture (..), Point (..),
                                                     Vector (..), circleSolid,
                                                     color, pictures, polygon,
                                                     rotate, translate, text, scale)
import           Graphics.Gloss.Data.Vector         (mulSV, unitVectorAtAngle, rotateV)
import           Graphics.Gloss.Interface.Pure.Game (Event (..), Key (..),
                                                     KeyState (..),
                                                     SpecialKey (..), play)
import           System.Random                      (StdGen (..), getStdGen,
                                                     mkStdGen, randomR,
                                                     randomRs, randoms)

data Shot = Shot {
        _s_position :: Point,
        _s_speed    :: Vector,
        _time_left  :: Float
    } deriving (Eq, Show)

makeLenses ''Shot

data Ship = Ship {
        _position     :: Point,
        _speed        :: Vector,
        _acceleration :: Float,
        _angle        :: Float,
        _angular      :: Float,
        _dead         :: Bool,
        _shape        :: [Point],
        _shots        :: [Shot],
        _last_shoot   :: Float,
        _shooting     :: Bool
    } deriving (Eq, Show)

makeLenses ''Ship

data Asteroid = Asteroid {
        _a_position :: Point,
        _a_speed    :: Vector,
        _a_angle    :: Float,
        _a_angular  :: Float,
        _a_shape    :: [Point]
    } deriving (Eq, Show)

makeLenses ''Asteroid

data World = World {
        _ship      :: Ship,
        _asteroids :: [Asteroid],
        _score     :: Int,
        _max_score :: Int
    } deriving (Eq, Show)

makeLenses ''World

main :: IO ()
main = do
    gen <- getStdGen
    play
        window
        background
        60
        (initial gen)
        draw
        inputs
        update

width :: Int
width = 720

height :: Int
height = 720

window :: Display
window = InWindow "Asteroids" (width, height) (300, 10)

background :: Color
background = makeColor 0.2 0.2 0.25 1.0

initialShip :: Ship
initialShip = Ship (0.0, 0.0) (0.0, 0.0) 0.0 0.0 0.0 False [
        (0.0,    25.0),
        (15.0,  -20.0),
        (0.0,   -10.0),
        (-15.0, -20.0)
    ] [] 10 False

initial :: StdGen -> World
initial gen = World
    initialShip
    (take n [makeAsteroid $ mkStdGen g | g <- randoms gen])
    0
    n
    where
        n :: Int
        n = (fst $ randomR (4, 8) gen)

draw :: World -> Picture
draw world = pictures $ (map drawAsteroid $ world ^. asteroids)
    ++ [drawShip $ world ^. ship]
    ++ (map drawShot $ world ^. ship . shots)
    ++ [scoreView]
    where
        scoreView :: Picture
        scoreView =
            translate (fromIntegral width/2 - 30) (fromIntegral height/2 - 30)
            $ Color white
            $ scale 0.2 0.2
            $ text
            $ show (world ^. score)

drawShip :: Ship -> Picture
drawShip Ship{_position=(x, y), _angle=a, _dead=d, _shape=sh} = translate x y
    $ rotate a
    $ if d
        then color (makeColor 1.0 1.0 1.0 1.0) $ polygon sh
        else color (makeColor 0.6 0.2 0.1 1.0) $ polygon sh

drawAsteroid :: Asteroid -> Picture
drawAsteroid Asteroid{_a_position=(x, y), _a_angle=a, _a_shape=shape} = translate x y
    $ rotate a
    $ color (makeColor 0.45 0.55 0.49 1.0)
    $ polygon shape

drawShot :: Shot -> Picture
drawShot Shot{_s_position=(x, y)} = translate x y
    $ color (makeColor 0.2 0.7 0.4 1.0)
    $ circleSolid 2.0

makeAsteroid :: StdGen -> Asteroid
makeAsteroid g0 = Asteroid (x, y) (dx, dy) 0 da shape
    where
        (x,  g1) = let w = fromIntegral width  / 2 in randomR (-w, w) g0
        (y,  g2) = let h = fromIntegral height / 2 in randomR (-h, h) g1

        (dx, g3) = randomR (-50.0, 50.0) g2
        (dy, g4) = randomR (-50.0, 50.0) g3

        (da, g5) = randomR (-200.0, 200.0) g4

        -- edges
        (e,  g6) = randomR (3, 7) g5
        shape = [let a = 2*i*pi/e in (r * cos a, r * sin a) | (i, r) <- zip [0..e] $ randomRs (10.0, 50.0) g6]

inputs :: Event -> World -> World
inputs (EventKey (SpecialKey KeyLeft)  Down _ _) = ship . angular      -~ 180.0
inputs (EventKey (SpecialKey KeyRight) Down _ _) = ship . angular      +~ 180.0
inputs (EventKey (SpecialKey KeyLeft)  Up   _ _) = ship . angular      +~ 180.0
inputs (EventKey (SpecialKey KeyRight) Up   _ _) = ship . angular      -~ 180.0
inputs (EventKey (SpecialKey KeyUp)    Down _ _) = ship . acceleration +~   2.0
inputs (EventKey (SpecialKey KeyUp)    Up   _ _) = ship . acceleration -~   2.0
inputs (EventKey (SpecialKey KeyDown)  Down _ _) = ship . acceleration -~   2.0
inputs (EventKey (SpecialKey KeyDown)  Up   _ _) = ship . acceleration +~   2.0
inputs (EventKey (SpecialKey KeySpace) Down _ _) = ship                %~ shoot
inputs (EventKey (Char       'r')      Down _ _) = initial . mkStdGen . getNewSeed
inputs (EventKey (Char       'R')      Down _ _) = initial . mkStdGen . getNewSeed
inputs _                                         = id

getNewSeed :: World -> Int
getNewSeed w = sum . map floor $ [
        w ^. ship . position . _1,
        w ^. ship . position . _2,
        w ^. ship . speed    . _1,
        w ^. ship . speed    . _2,
        w ^. ship . angle
    ] ++ concatMap (\ast -> [
            ast ^. a_position . _1,
            ast ^. a_position . _2,
            ast ^. a_speed    . _2,
            ast ^. a_speed    . _2
        ]) (w ^. asteroids)

update :: Float -> World -> World
update dt w =
    (ship %~ updateShip dt (w ^. asteroids))
    . (score %~ const (w ^. max_score - length (w ^. asteroids)))
    . (asteroids %~ catMaybes . map (updateAsteroid dt (w ^. ship)))
    $ w

updateShip :: Float -> [Asteroid] -> Ship -> Ship
updateShip dt asts s =
    (if s ^. dead
        then id
        else
            (angle      +~ dt * s ^. angular)
            . (position %~ inScreen . modifyPosition (s ^. speed) dt)
            . (speed    %~ updateSpeed (s ^. angle) (s ^. acceleration)))
    . (shots %~ catMaybes . map (updateShot dt asts))
    . (dead  %~ checkCollision s asts)
    . (shooting %~ const False)
    . (last_shoot %~ \t -> if s ^. shooting then 0 else t + dt)
    $ s

modifyPosition :: Vector -> Float -> Point -> Point
modifyPosition (dx, dy) dt = flip translatePoint (dt * dx, dt * dy)

translatePoint :: Point -> Vector -> Point
translatePoint (x, y) (dx, dy) = (x + dx, y + dy)

inScreen :: Point -> Point
inScreen (x, y) = (inside x width, inside y height)
    where
        inside :: Float -> Int -> Float
        inside p s
            | p > l / 2  = p - l
            | p < -l / 2 = p + l
            | otherwise  = p
            where
                l :: Float
                l = fromIntegral s

updateSpeed :: Float -> Float -> Vector -> Vector
updateSpeed a r v = translatePoint (mulSV r . unitVectorAtAngle . toRadians $ a) v

toRadians :: Float -> Float
toRadians a = (pi / 2) - pi * a / 180

updateAsteroid :: Float -> Ship -> Asteroid -> Maybe Asteroid
updateAsteroid dt s ast = if hit
    then Nothing
    else
        Just $ (a_angle +~ dt * ast ^. a_angular)
        . (a_position %~ inScreen . modifyPosition (ast ^. a_speed) dt)
        $ ast
    where
        hit :: Bool
        hit = or . map (\p -> pointInside p (getShapeAst ast)) . map _s_position $ s ^. shots

getShapeAst :: Asteroid -> [Point]
getShapeAst ast = map (translatePoint $ ast ^. a_position) (ast ^. a_shape)

getShape :: Ship -> [Point]
getShape s = map (translatePoint $ s ^. position) (s ^. shape)

checkCollision :: Ship -> [Asteroid] -> Bool -> Bool
checkCollision s asts = (||) $ or
    . map (\points -> or . map (\p -> pointInside p points) $ getShape s)
    . map getShapeAst
    $ asts

pointInside :: Point -> [Point] -> Bool
pointInside (x, y) shape = odd . length . filter check . zip shape $ rotate shape
    where
        rotate :: [a] -> [a]
        rotate []     = []
        rotate (x:xs) = xs ++ [x]

        check :: (Point, Point) -> Bool
        check ((ax, ay), (bx, by))
            | (ay > y) == (by > y)                      = False
            | x < (bx - ax) * (y - ay) / (by - ay) + ax = True
            | otherwise                                 = False

shoot :: Ship -> Ship
shoot s = if s ^. dead || s ^. last_shoot < 1.0
    then s
    else  (shooting %~ const True)
        . (shots %~ (newShot :))
        $ s
    where
        newShot :: Shot
        newShot = Shot
            (translatePoint (s ^. position) $ rotateV (toRadians $ s ^. angle + 90) $ (s ^. shape) !! 0)
            (mulSV 200.0 . unitVectorAtAngle . toRadians $ s ^. angle)
            2.0

updateShot :: Float -> [Asteroid] -> Shot -> Maybe Shot
updateShot dt asts s = if (s ^. time_left) < 0 || hits
    then Nothing
    else Just $ (s_position %~ inScreen . modifyPosition (s ^. s_speed) dt)
        . (time_left -~ dt) $ s
    where
        hits :: Bool
        hits = any (pointInside $ s ^. s_position) . map getShapeAst $ asts
