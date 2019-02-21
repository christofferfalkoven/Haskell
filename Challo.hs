rollDice :: IO Int
rollDice = getStdRandom (randomR (1,6))
