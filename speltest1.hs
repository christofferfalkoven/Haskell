
import Graphics.Gloss


width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black


data PongGame = Game
  { ballLoc :: (Float, Float)  -- ^ Pong ball (x, y) location.
  , ballVel :: (Float, Float)  -- ^ Pong ball (x, y) velocity.
  , player1 :: Float           -- ^ Left player paddle height.
                               -- Zero is the middle of the screen.
  , player2 :: Float           -- ^ Right player paddle height.
  } deriving Show

initialState :: PongGame
initialState = Game
   { ballLoc = (-10, 30)
   , ballVel = (1, -3)
   , player1 = 40
   , player2 = -80
   }


drawing :: Picture
drawing = pictures [ball, walls,
                    mkPaddle rose 120 (-20),
                    mkPaddle orange (-120) 40]
  where
    --  The pong ball.
    ball = translate (-10) 40 $ color ballColor $ circleSolid 10
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

    -- | Convert a game state into a picture.
    render :: PongGame  -- ^ The game state to render.
           -> Picture   -- ^ A picture of this game state.
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

        -- | Update the ball position using its current velocity.
moveBall :: Float    -- ^ The number of seconds since last update
         -> PongGame -- ^ The initial game state
         -> PongGame -- ^ A new game state with an updated ball position

moveBall seconds game = game { ballLoc = (x', y') }
   where
             -- Old locations and velocities.
     (x, y) = ballLoc game
     (vx, vy) = ballVel game

             -- New locations.
     x' = x + vx * seconds
     y' = y + vy * seconds

     -- | Detect a collision with a paddle. Upon collisions,
     -- change the velocity of the ball to bounce it off the paddle.
paddleBounce :: PongGame -> PongGame

     -- | Detect a collision with one of the side walls. Upon collisions,
     -- update the velocity of the ball to bounce it off the wall.
wallBounce :: PongGame -> PongGame

type Radius = Float
type Position = (Float, Float)

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

     -- | Run a finite-time-step simulation in a window.


fps :: Int
fps = 60

main :: IO ()
main = play window background fps initialState render handleKeys update
    -- | Update the game by moving the ball.
     -- Ignore the ViewPort argument.
