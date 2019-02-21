import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort
import Data.Monoid
import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import System.Random
--import System.Random

data Challo = Game {trunks1 :: (Float, Float), trunks2 :: (Float, Float), trunks3 :: (Float, Float),           -- trunkx (trunklocation y, with gaplocation x)
  trunks4 :: (Float, Float), trunkSpeed :: Float, playerLocation :: Float} deriving Show

initialState = Game {trunks1 = (300, 0), trunks2 = (450, 0), trunks3 = (600, 0), trunks4 = (750, 0), trunkSpeed = (-100), playerLocation = 0}

width,height,offset:: Int
width = 400
height = 600
offset = 100

window :: Display
window = InWindow "Challo" (width, height) (offset, offset)

background :: Color
background = blue

updateFrame :: Challo -> Picture
updateFrame game = pictures [trunk1, trunk2, trunk3, trunk4, player, walls]
    where
      player = translate (playerLocation game) (-270) (color black (rectangleSolid 20 20))
      trunk1 = pictures [translate 0 (fst(trunks1 game)) (color green (rectangleSolid 390 20)),
                        translate (snd(trunks1 game)) (fst(trunks1 game)) (color blue (rectangleSolid 40 20))]
      trunk2 = pictures [translate 0 (fst(trunks2 game)) (color green (rectangleSolid 390 20)),
                        translate (snd(trunks2 game)) (fst(trunks2 game)) (color blue (rectangleSolid 40 20))]
      trunk3 = pictures [translate 0 (fst(trunks3 game)) (color green (rectangleSolid 390 20)),
                        translate (snd(trunks3 game)) (fst(trunks3 game)) (color blue (rectangleSolid 40 20))]
      trunk4 = pictures [translate 0 (fst(trunks4 game)) (color green (rectangleSolid 390 20)),
                        translate (snd(trunks4 game)) (fst(trunks4 game)) (color blue (rectangleSolid 40 20))]
      walls = pictures [wall (-200), wall 200]
          where
            wall :: Float -> Picture
            wall offset = translate offset 0 (color black (rectangleSolid 10 600))

moveTrunk :: Float -> Challo -> Challo
moveTrunk seconds game = game {trunks1 = a', trunks2 = b', trunks3 = c', trunks4 = d'}
  where
    ts = trunkSpeed game
    (a'',_) = trunks1 game
    a' | a'' < (-300) = (300, 0)
       | otherwise = ((a'' + ts*seconds), 0)
    (b'',_) = trunks2 game
    b' | b'' < (-300) = (300, 0)
       | otherwise = ((b'' + ts*seconds), 0)
    (c'',_) = trunks3 game
    c' | c'' < (-300) = (300, 0)
       | otherwise = ((c'' + ts*seconds), 0)
    (d'',_) = trunks4 game
    d' | d'' < (-300) = (300, 0)
       | otherwise = ((d'' + ts*seconds), 0)

collision :: Challo -> Challo
collision game | (fst (trunks1 game)) <= (-260) && (fst (trunks1 game)) >= (-280) =
                if (((snd (trunks1 game)) + 10) > (playerLocation game) && ((snd (trunks1 game)) - 10) < (playerLocation game)) then game
                else error "You Suck! ballz"
               | (fst (trunks2 game)) <= (-260) && (fst (trunks2 game)) >= (-280) =
                if (((snd (trunks2 game)) + 10) > (playerLocation game) && ((snd (trunks2 game)) - 10) < (playerLocation game)) then game
                else error "You Suck! ballz"
               | (fst (trunks3 game)) <= (-260) && (fst (trunks3 game)) >= (-280) =
                if (((snd (trunks3 game)) + 10) > (playerLocation game) && ((snd (trunks3 game)) - 10) < (playerLocation game)) then game
                else error "You Suck! ballz"
               | (fst (trunks4 game)) <= (-260) && (fst (trunks4 game)) >= (-280) =
                if (((snd (trunks4 game)) + 10) > (playerLocation game) && ((snd (trunks4 game)) - 10) < (playerLocation game)) then game
                else error "You Suck! ballz"
               | otherwise = game


-- | Respond to key events.
keyTouch :: Event -> Challo -> Challo
keyTouch (EventKey (Char 'a') Down _ _) game =
  game { playerLocation = ((playerLocation game) - 20)  }
keyTouch (EventKey (Char 'd') Down _ _ ) game =
  game { playerLocation = ((playerLocation game) + 20)  }
keyTouch _ game = game

fps :: Int
fps = 60
{-}
createRandom :: IO Float
createRandom = do
  x <- randomRIO (-190, 190)
  print (x :: Float)
-}
randomNumber :: IO Int
randomNumber = getStdRandom (randomR (-190, 190))

--randomNumberFloat:: IO Int  ->  Float
{-
comboRandom :: Float
comboRandom = do
  newRandom <- createRandom
  show (convertRandom newRandom)
-}


main :: IO ()
main = play window background fps initialState updateFrame keyTouch update
  where
    update :: Float -> Challo -> Challo

    update seconds game = moveTrunk seconds (collision game)
