import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort

type Radius = Float
type Position = (Float, Float)



-- | Data describing the state of the pong game.
data PongGame = Game
  { ballLoc :: (Float, Float)  -- ^ Pong ball (x, y) location.
  , ballVel :: (Float, Float)  -- ^ Pong ball (x, y) velocity.
  , player1 :: Float           -- ^ Left player paddle height.
  , player2 :: Float           -- ^ Right player paddle height.
  } deriving Show


  -- | The starting state for the game of Pong.
initialState :: PongGame
initialState = Game
  { ballLoc = (-10, 30)
  , ballVel = (8, -24)
  , player1 = 40
  , player2 = -80
  }

width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

-- | Convert a game state into a picture.
render :: PongGame -> Picture   -- ^ A picture of this game state.
render game =
  pictures [ball, walls,
            mkPaddle rose 120 $ player1 game,
            mkPaddle orange (-120) $ player2 game]
  where
    --  The pong ball.
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
    ballColor = dark red

    --  The bottom and top walls.
    wall :: Float -> Picture
    wall offset =
      translate 0 offset $
        color wallColor $
          rectangleSolid 270 10

    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150)]

    --  Make a paddle of a given border and vertical offset.
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures
      [ translate x y $ color col $ rectangleSolid 26 86
      , translate x y $ color paddleColor $ rectangleSolid 20 80
      ]

    paddleColor = light (light blue)

moveBall seconds game = game { ballLoc = (x', y') }
  where
    (x, y) = ballLoc game
    (vx, vy) = ballVel game
    x' = x + vx * seconds
    y' = y + vy * seconds


-- | Given position and radius of the ball, return whether a collision occurred.
wallCollision :: Position -> Radius -> Bool
wallCollision (_, y) radius = topCollision || bottomCollision
  where
    topCollision    = y - radius <= -fromIntegral width / 2
    bottomCollision = y + radius >=  fromIntegral width / 2

wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx, vy') }
  where
  -- Radius. Use the same thing as in `render`.
    radius = 10
  -- The old velocities.
    (vx, vy) = ballVel game
    vy' = if wallCollision (ballLoc game) radius
          then
         -- Update the velocity.
           -vy
          else
            -- Do nothing. Return the old velocity.
           vy

    -- | Number of frames to show per second.
fps :: Int
fps = 60

main :: IO ()
main = simulate window background fps initialState render update

-- | Update the game by moving the ball and bouncing off walls.
update :: ViewPort -> Float -> PongGame -> PongGame
update _ seconds = wallBounce . moveBall seconds
