import Random

main =
    do { let g = mkStdGen 42
       ; let [s] = take 1 (randomStuff g)       
       ; print s
       }

randomStuff :: RandomGen g => g -> [[Float]]
randomStuff g = work (randomRs (0.0, 1.0) g)

work :: [Float] -> [[Float]]
work (r:rs) =
    let n = truncate (r * 7.0) + 1
        (xs,ys) = splitAt n rs
    in xs : work ys
