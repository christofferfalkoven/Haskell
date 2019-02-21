import Graphics.Gloss
import Graphics.Gloss.Data.Picture

width, height, offset :: Int
width = 400
height = 600
offset = 100

window :: Display
window = InWindow "LoveAlch" (width, height) (offset, offset)

background:: Color
background = white

data LoveAlchGame = Game
  { playerLocation :: (Float, Float), playerSpeed :: (Float, Float), player :: Float } deriving Show

initialState :: LoveAlchGame
initialState = Game
     { playerLocation = (-10, 30), playerSpeed = (1, -3), player = 40}

drawing :: Picture
drawing = pictures [ball, walls]
       where
         --  The pong ball.
         ball = translate (-200) 40 $ color ballColor $ circleSolid 5
         ballColor = dark red

         wall :: Float -> Picture
         wall offset =
           translate 100 offset $
             color wallColor $
               rectangleSolid 10 270


         wallColor = greyN 0.5
         walls = pictures [wall 150, wall (-150)]

main :: IO ()
main = display window background drawing




--bana = display (InWindow "Vårt spel" (1000, 1000) (10, 10)) white rectangle1

--rectangle1 = pictures [(rectangleSolid 200 5),(translate 250 0 (rectangleSolid 200 5))]
