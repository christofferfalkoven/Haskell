import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import System.IO.Unsafe

{- REPRESENTATION CONVENTION: This data represents all the different variables in the game ranging from the (x, y) coordinates of the trunks, the gaps in them,
                              the stars, both the start and the end screens and finally the player. Also the highschore accumilates there along with the amount of extra life
                              and the KeyStates of both the buttons.
   REPRESENTATION INVARIANT:  If trunkspeed reaches < (-600) then each trunk falls the length of the window each second meaning that all the trunks seeming stack on the top of the screen.
                              The highScore window can only hold 3 digits so the highest highscore you can get without it going out the white box is 999.
 -}

data Challo = Game {trunks1 :: (Float, Float),
                    trunks2 :: (Float, Float),
                    trunks3 :: (Float, Float),
                    trunks4 :: (Float, Float),
                    star1 :: (Float, Float),
                    starColor :: Color,
                    trunkSpeed :: Float,
                    playerLocation :: Float,
                    highScore :: Int,
                    aButton :: KeyState,
                    dButton :: KeyState,
                    stateOfGame :: Int,
                    extraLife :: (Int, Int),
                    startScreen :: (Float, Float),
                    endScreen :: (Float, Float)} deriving Show

initialState = Game {trunks1 = (300, unsafePerformIO randomNumber),
                     trunks2 = (450, unsafePerformIO randomNumber),
                     trunks3 = (600, unsafePerformIO randomNumber),
                     trunks4 = (750, unsafePerformIO randomNumber),
                     star1 = (675, unsafePerformIO randomNumber),
                     starColor = yellow,
                     trunkSpeed = (0),
                     playerLocation = 0,
                     highScore = 0,
                     aButton = Up,
                     dButton = Up,
                     stateOfGame = 0,
                     extraLife = (0, 0),
                     startScreen = ((-175),0),
                     endScreen = (4910,5000)}

width,height,offset:: Int
width = 400
height = 600
offset = 100

{- Window : Display
   PRE: Width, hight and offset are all Int's
   POST: Creates a Window with the size hight x width called "Challo"
   SIDE EFFECT: Displays a Window
-}

window :: Display
window = InWindow "Challo" (width, height) (offset, offset)

background :: Color
background = blue

{- updateFrame :: Challo -> Picture
   PRE:  All values used for picture are in the type Float
   POST: Takes the current state of Challo and displays it as a graphical picture
   SIDE EFFECTS: Shows a rendering of the current state of Challo.
-}

updateFrame :: Challo -> Picture
updateFrame game = pictures [trunk1, trunk2, trunk3, trunk4, starPictures, hssquare, player, walls, highscore, startwindow, endwindow]
    where
      startwindow = pictures [translate (fst(startScreen game)) (snd(startScreen game)) (color white (rectangleSolid 900 1200)),
            translate (fst(startScreen game)) (snd(startScreen game)) (scale 0.3 0.3 (color black (text "Welcome to Challo!"))),
            translate (fst(startScreen game+100)) (snd(startScreen game)+(-50)) (scale 0.15 0.15 (color black (text "Press y to start.")))]
      endwindow = pictures [translate (fst(endScreen game)) (snd(endScreen game)) (color black (rectangleSolid 900 1200)),
            translate (fst(endScreen game)) (snd(endScreen game)) (scale 0.5 0.5 (color red (text "Defeat"))),
            translate (fst(endScreen game)) (snd(endScreen game)+(-50)) (scale 0.15 0.15 (color red (text ("Your score: "++(show (highScore game)))))),
            translate (fst(endScreen game)) (snd(endScreen game)+(-80)) (scale 0.15 0.15 (color red (text "Play again? press y."))),
            translate (fst(endScreen game)) (snd(endScreen game)+(-110)) (scale 0.15 0.15 (color red (text "To quit, press esc.")))]
      highscore = translate 130 250 (scale 0.2 0.2 (color black (text (show (highScore game)))))
      hssquare = translate 150 260 (color white (rectangleSolid 55 30))
      player = translate (playerLocation game) (-270) (color black (rectangleSolid 20 20))
      trunk1 = pictures [translate 0 (fst(trunks1 game)) (color green (rectangleSolid 390 20)),
                        translate (snd(trunks1 game)) (fst(trunks1 game)) (color blue (rectangleSolid 65 20))]
      trunk2 = pictures [translate 0 (fst(trunks2 game)) (color green (rectangleSolid 390 20)),
                        translate (snd(trunks2 game)) (fst(trunks2 game)) (color blue (rectangleSolid 65 20))]
      trunk3 = pictures [translate 0 (fst(trunks3 game)) (color green (rectangleSolid 390 20)),
                        translate (snd(trunks3 game)) (fst(trunks3 game)) (color blue (rectangleSolid 65 20))]
      trunk4 = pictures [translate 0 (fst(trunks4 game)) (color green (rectangleSolid 390 20)),
                        translate (snd(trunks4 game)) (fst(trunks4 game)) (color blue (rectangleSolid 65 20))]
      walls = pictures [wall (-200), wall 200]
          where
            wall :: Float -> Picture
            wall offset = translate offset 0 (color black (rectangleSolid 10 600))
      starPictures = pictures [translate 140 220 (scale 0.3 0.3 (color yellow star)), translate 160 210 (scale 0.2 0.2 (color black (text (show (fst(extraLife game)))))),
          translate (snd(star1 game)) (fst(star1 game)) (scale 0.4 0.4 (color (starColor game) star))]
            where
              star = polygon [
                    ( 10,  20),
                    ( 40,  20),
                    ( 20,   0),
                    ( 30, -30),
                    (  0, -10),
                    (-30, -30),
                    (-20,   0),
                    (-40,  20),
                    (-10,  20),
                    (  0,  50)]

{- updateGameSpeed :: Challo -> Challo
   PRE: trunkSpeed should be over (-600) for the game work reasonably well.
        trunkSpeed must be negative for it to go down instead of up.
   POST: Increases the speed of the trunks exponentionally for every fifth trunk
   SIDE EFFECTS: Updates the speed of the trunks
-}

updateGameSpeed :: Challo -> Challo
updateGameSpeed game = game {trunkSpeed = x'}
  where
    a = highScore game
    x = trunkSpeed game
    x' | a == 0 = x
       | (mod a 5 == 0) && (((trunks1 game) < (-300)) || ((trunks2 game) < (-300))
                        || ((trunks3 game) < (-300)) || ((trunks4 game) < (-300))) = x*1.02   -- increasing trunkspeed with 2% every 5th trunk.
       | otherwise = x

{- updatehighScore :: Challo -> Challo
  PRE: True
  POST: Updates the current score by one for every trunk you pass
  SIDE EFFECTS: Shows an increase in your score
-}

updatehighScore :: Challo -> Challo
updatehighScore game = game {highScore = a'}               -- for each layer you succed, highscore increses by 1.
  where
    a = highScore game
    a'= if ((trunks1 game) < (-300)) || ((trunks2 game) < (-300)) ||
        ((trunks3 game) < (-300)) || ((trunks4 game) < (-300))
         then a+1
         else a

{- moveObjects :: Float -> Challo -> Challo
    PRE:
    POST: Move the trunks down and then when they reach the lowest point of our
    window moves them up to the top of our window.
    Also updates the  star and adds +1 for every star you collide with.
    SIDE EFFECTS: Changes the position of all objects except the player. also updates the starColor and the extra life meter.
-}

moveObjects :: Float -> Challo -> Challo
moveObjects seconds game = game {trunks1 = a', trunks2 = b', trunks3 = c', trunks4 = d', star1 = e', starColor = f, extraLife = (x,y)}
  where
     ts = trunkSpeed game
     (a,_) = trunks1 game
     a' | a < (-300) = (300, unsafePerformIO randomNumber)
        | otherwise = ((a + ts*seconds), (snd (trunks1 game)))
     (b,_) = trunks2 game
     b' | b < (-300) = (300, unsafePerformIO randomNumber)
        | otherwise = ((b + ts*seconds), (snd (trunks2 game)))
     (c,_) = trunks3 game
     c' | c < (-300) = (300, unsafePerformIO randomNumber)
        | otherwise = ((c + ts*seconds), (snd (trunks3 game)))
     (d,_) = trunks4 game
     d' | d < (-300) = (300, unsafePerformIO randomNumber)
        | otherwise = ((d + ts*seconds), (snd (trunks4 game)))
     (e,_) = star1 game
     e' | e < (-300) = (300, unsafePerformIO randomNumber)
        | otherwise = ((e + ts*seconds), (snd (star1 game)))
     f = if e < (-300)
          then yellow
          else starColor game
     (x,y)  | (a < (-300)) || (b < (-300)) || (c < (-300)) || (d < (-300)) = (fst(extraLife game), 0)
            | otherwise = extraLife game

{- collision :: Challo -> Challo
  PRE: True
  POST: If you have collided with an object return either a loss of a life, an addition of a life or ends the game.
  SIDE EFFECTS: Either ends the game or returns an increase of a decrease in your lifetotal.
-}

collision :: Challo -> Challo
collision game | (fst (trunks1 game)) <= (-260) && (fst (trunks1 game)) >= (-280) =
                if (((snd (trunks1 game)) + 30) > (playerLocation game) && ((snd (trunks1 game)) - 30) < (playerLocation game))
                 then game
                 else looseLife game
               | (fst (trunks2 game)) <= (-260) && (fst (trunks2 game)) >= (-280) =
                if (((snd (trunks2 game)) + 30) > (playerLocation game) && ((snd (trunks2 game)) - 30) < (playerLocation game))
                 then game
                 else looseLife game
               | (fst (trunks3 game)) <= (-260) && (fst (trunks3 game)) >= (-280) =
                if (((snd (trunks3 game)) + 30) > (playerLocation game) && ((snd (trunks3 game)) - 30) < (playerLocation game))
                 then game
                 else looseLife game
               | (fst (trunks4 game)) <= (-260) && (fst (trunks4 game)) >= (-280) =
                if (((snd (trunks4 game)) + 30) > (playerLocation game) && ((snd (trunks4 game)) - 30) < (playerLocation game))
                 then game
                 else looseLife game
               | (fst (star1 game)) <= (-260) && (fst (star1 game)) >= (-280) =
                if (((snd (star1 game)) + 23) > (playerLocation game) && ((snd (star1 game)) - 23) < (playerLocation game))
                 then  if (starColor game) == yellow
                        then let (x,y) = extraLife game
                             in game {starColor = blue, extraLife = ((x+1),y)}
                        else game {starColor = blue}
                 else game
               | otherwise = game

{- looseLife :: Challo -> Challo
   PRE: True
   POST: Gets called if you have collided with a trunk.
   Either you loose one of you lives or the endScreen gets brought up
   SIDE EFFECTS: Shows an decrease in your score or the end screen
-}

looseLife :: Challo -> Challo
looseLife game | (x >= 1) && (y == 0) = game {extraLife = (x-1, 1)}
               | y == 1 = game
               | otherwise = endScreenGame game
                    where (x,y) = extraLife game

{- endScreenGame :: Challo -> Challo
  PRE: True
  POST: Moves the end game screen
  SIDE EFFECTS: Shows you the end screen.
-}

endScreenGame :: Challo -> Challo
endScreenGame game = game {endScreen = ((-90), 0), trunkSpeed = 0}

{- movePlayerLeft :: Challo -> Challo
  PRE: True
  POST: if the aButton key is equal to down move the player 4 pixels to the left
  SIDE EFFECTS: moves the player left
-}

movePlayerLeft :: Challo -> Challo
movePlayerLeft game | (aButton game) == Down = if (playerLocation game) <= (-205)
                                                then game {playerLocation = 204}
                                                else game {playerLocation = ((playerLocation game) - 4)}
                    | otherwise = game

{- movePlayerRight :: Challo -> Challo
   PRE: True
   POST: if the dButton key is equal to down move the player 4 pixels to the right
   SIDE EFFECTS: moves the player right
-}

movePlayerRight :: Challo -> Challo
movePlayerRight game | (dButton game) == Down = if (playerLocation game) >= (205)
                                                 then game {playerLocation = (-204)}
                                                 else game {playerLocation = ((playerLocation game) + 4)}
                     | otherwise = game

{- keyTouch :: Event -> Challo -> Challo
   PRE: True
   POST: If either the a, d, left or right key are held down the KeyState of the a or d button is changed in Challo
   SIDE EFFECTS: Returns if a button is held down or not.
-}

keyTouch :: Event -> Challo -> Challo
keyTouch (EventKey (Char 'a') Down _ _) game = game {aButton = Down}
keyTouch (EventKey (Char 'a') Up _ _) game = game {aButton = Up}
keyTouch (EventKey (Char 'd') Down _ _ ) game = game {dButton = Down}
keyTouch (EventKey (Char 'd') Up _ _ ) game = game {dButton = Up}
keyTouch (EventKey (SpecialKey KeyLeft) Down _ _) game = game {aButton = Down}
keyTouch (EventKey (SpecialKey KeyLeft) Up _ _) game = game {aButton = Up}
keyTouch (EventKey (SpecialKey KeyRight) Down _ _ ) game = game {dButton = Down}
keyTouch (EventKey (SpecialKey KeyRight) Up _ _ ) game = game {dButton = Up}
keyTouch (EventKey (Char 'y') Down _ _ ) game
  | (stateOfGame game) == 0 = game {trunkSpeed = (-100), startScreen = (5000, 5000), stateOfGame = 1}
  | otherwise = game {trunks1 = (300, unsafePerformIO randomNumber), trunks2 = (450, unsafePerformIO randomNumber),
                trunks3 = (600, unsafePerformIO randomNumber), trunks4 = (750, unsafePerformIO randomNumber),
                trunkSpeed = (-75), playerLocation = 0, highScore = 0, aButton = Up, dButton = Up,
                star1 = (675, 0), starColor = yellow, stateOfGame = 0, endScreen = (4910,5000) }
keyTouch _ game = game


fps :: Int
fps = 60

{- main :: IO ()
   PRE: True
   POST: Calls the play function which is a function built into GLOSS. This the calls the update function which calls an update 60 times in a second
   SIDE EFFECTS: Play the game
-}

main :: IO ()
main = play window background fps initialState updateFrame keyTouch update
  where
    update :: Float -> Challo -> Challo
    update seconds game = moveObjects seconds (collision (updatehighScore  (updateGameSpeed (movePlayerRight (movePlayerLeft game)))))

{- randomNumber :: (IO Float)
   PRE: True
   POST: Returns a random number between (-190) and 190
   SIDE EFFECTS: Takes a random number from the outside world and returns it.
-}

randomNumber::(IO Float)
randomNumber = (getStdRandom(randomR (-190, 190)))
