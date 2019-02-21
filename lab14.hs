main :: IO()
main = putChar('\n')
{-- REPRESENTATION CONVENTION: represents a playing card suit --}
data Suit =  Diamond | Club | Heart | Spade  deriving (Eq, Enum, Ord)
{-- REPRESENTATION CONVENTION: represents a playing card value --}
data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Enum, Ord)
{-- REPRESENTATION CONVENTION: represents a playing card, consisting of a Suit and a Value --}
data Card = Card Suit Value
               
  
greaterCard (Card suit1 value1) (Card suit2 value2)
  | value1 == value2 = suit1 > suit2
  | otherwise = value1 > value2
  
(+?) :: Maybe Int -> Maybe Int -> Maybe Int
(+?) (Just a) (Just b) = Just (a + b)
(+?) _ _= Nothing

(-?) :: Maybe Int -> Maybe Int -> Maybe Int
(-?) (Just a) (Just b) = Just (a - b)
(-?) _ _ = Nothing

(*?) :: Maybe Int -> Maybe Int -> Maybe Int
(*?) (Just a) (Just b) = Just (a * b)
(*?) _ _ = Nothing

(/?) :: Maybe Int -> Maybe Int -> Maybe Int
(/?) (Just a) (Just b)
  | b == 0 = Nothing 
  | otherwise = Just (a `div` b)
(/?) _ _ = Nothing
