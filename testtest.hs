import System.Random

main = randomRIO (1,100) >>= print
