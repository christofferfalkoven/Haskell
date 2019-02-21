import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort
import System.Random

data Challo = Game
 {trunkLocation :: Float, trunkSpeed :: Float, playerLocation :: Float} deriving Show

initialState = Game {trunkLocation = 200, trunkSpeed = (-100), playerLocation = 0}

width,height,offset:: Int
width = 400
height = 600
offset = 100

window :: Display
window = InWindow "Challo" (width, height) (offset, offset)

background :: Color
background = blue

updateFrame :: Challo -> Picture
updateFrame game = pictures [player, walls, trunks]
    where
      player = translate (playerLocation game) (-270) (color black (rectangleSolid 20 20))
      trunks = pictures [translate 102.5 (trunkLocation game) (color green (rectangleSolid 185 20)),
                        translate (-122.5) (trunkLocation game) (color green (rectangleSolid 145 20))]

      walls = pictures [wall (-200), wall 200]
          where
            wall :: Float -> Picture
            wall offset = translate offset 0 (color black (rectangleSolid 10 600))

moveTrunk :: Float -> Challo -> Challo
moveTrunk seconds game = game {trunkLocation = y'}
  where
    y = trunkLocation game
    ts = trunkSpeed game
    y' = y + ts*seconds

-- | Respond to key events.
keyTouch :: Event -> Challo -> Challo
keyTouch (EventKey (Char 'a') Down _ _) game =
  game { playerLocation = ((playerLocation game) - 20)  }
keyTouch (EventKey (Char 'd') Down _ _ ) game =
  game { playerLocation = ((playerLocation game) + 20)  }
keyTouch _ game = game

fps :: Int
fps = 60

main :: IO ()
main = play window background fps initialState updateFrame keyTouch update
  where
    update :: Float -> Challo -> Challo
    update seconds game = moveTrunk seconds game

{-
trunks = pictures trunk

trunk :: Picture
trunk = [newTrunk] ++ [newTrunk]
where
newTrunk ::
 randomRIO (1, 3)


-}
