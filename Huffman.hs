--Dan Sandqvist och Christoffer Falkovén
-- DO NOT MODIFY THE FOLLOWING LINES

module Huffman(HuffmanTree, characterCounts, huffmanTree, codeTable, compress, decompress) where

import Table
import PriorityQueue

import Test.HUnit  -- if this causes an error, type 'cabal install HUnit' at the command line

{- REPRESENTATION CONVENTION:
     a bit code (of a character or string) is represented by a list of Booleans
   REPRESENTATION INVARIANT:
     the bit code is a concatenation of (0 or more) valid code words for some
     Huffman tree
 -}
type BitCode = [Bool]

-- END OF DO NOT MODIFY ZONE

--------------------------------------------------------------------------------

{- characterCounts s
   PURPOSE: given a string it returns a table that shows how manny times a given Char acures
   PRE:  True
   POST: a table that maps each character that occurs in s to the number of
         times the character occurs in s
   EXAMPLES: characterCounts "hello world" = T [('h',1),('e',1),('l',3),('o',2),(' ',1),('w',1),('r',1),('d',1)]
             characterCounts "xxx"         = T [('x',3)]
-}
characterCounts :: String -> Table Char Int
characterCounts s = characterCountsAux s Table.empty

{-characterCountsAux s t
  PRE:  t is an empty Table
  POST: a table that maps each character that occurs in s to the number of
        times the character occurs in s
  EXAMPLES: characterCountsAux "hello world"  Table.empty = T [('h',1),('e',1),('l',3),('o',2),(' ',1),('w',1),('r',1),('d',1)]
            characterCountsAux "xxx"          Table.empty = T [('x',3)]
-}
--VARIANT: The length of (x:ls)
characterCountsAux :: String -> Table Char Int -> Table Char Int
characterCountsAux []     t = t
characterCountsAux (x:ls) t
                | Table.exists t x = characterCountsAux ls t
                | otherwise = characterCountsAux ls ( Table.insert t x (length(filter (==x) (x:ls))) )


{- REPRESENTATION CONVENTION: a Tree that represents a trring it has two types of nodes:
  a leaf that has boath a Char and a Int depending on how manny times the char apears in the String that the Tree represents
  and a Node that keeps track of how many the combides Chars of diferent types if has under it
   REPRESENTATION INVARIANT: Int => 0
 -}
data HuffmanTree = Leaf (Char, Int) | Node HuffmanTree Int HuffmanTree deriving (Show,Eq)


{- huffmanTree t
   PURPOSE: builds a coresponding HuffmanTree to a given t
   PRE:  t maps each key to a positive value
   POST: a Huffman tree based on the character counts in t
   EXAMPLES: huffmanTree (characterCounts "hello world") = Node (Node (Node (Leaf (' ',1)) 2 (Leaf ('w',1))) 4 (Node (Leaf ('r',1)) 2 (Leaf ('d',1)))) 11 (Node (Leaf ('l',3)) 7 (Node (Leaf ('o',2)) 4 (Node (Leaf ('h',1)) 2 (Leaf ('e',1)))))
             huffmanTree (characterCounts "xxx") = Leaf ('x',3)
 -}
huffmanTree :: Table Char Int -> HuffmanTree
huffmanTree t = huffmanTreeAux (tableToPQHuffman t)

{-huffmanTreeAux pq
  PRE: pq all huffmanTreeAux have positive int values
  POST: combines the huffmantrees in the PriorityQueue untill there is only one left and returns it
  EXAMPLE: huffmanTreeAux ((Leaf ('a',1),1) : (Leaf ('b',1),1) : [])  = Node (Leaf ('a',1)) 2 (Leaf ('b',1))
           huffmanTreeAux (Node (Leaf ('a',1)) 2 (Leaf ('b',1)) : []) = Node (Leaf ('a',1)) 2 (Leaf ('b',1))
-}
--VARIANT: the length of the PriorityQueue HuffmanTree
huffmanTreeAux :: PriorityQueue HuffmanTree -> HuffmanTree
huffmanTreeAux []  = (Leaf (' ',0))
huffmanTreeAux ((x,p):[]) = x
huffmanTreeAux ( (h1,p1):(h2,p2):ls ) = huffmanTreeAux $ PriorityQueue.insert (ls) ( (Node (h1) (p1+p2) (h2)),(p1+p2) )

{-tableToPQHuffman t
  PRE: t maps each key to a positive value
  POST: converts a Table Char Int to a PriorityQueue of huffmantrees
-}
tableToPQHuffman ::Table Char Int -> PriorityQueue HuffmanTree
tableToPQHuffman t = pQCharToPQHuffman (PriorityQueue.empty) (tableToPQ t)

{-pQCharToPQHuffman hl cl
  PRE: hs is empty
  POST: converts a PriorityQueue of chars to a PriorityQueue of huffmantrees
-}
--VATIANT: the length of cl
pQCharToPQHuffman :: PriorityQueue HuffmanTree -> PriorityQueue Char -> PriorityQueue HuffmanTree
pQCharToPQHuffman hl  []         = hl
pQCharToPQHuffman hl ((c,i):pql) = pQCharToPQHuffman ( PriorityQueue.insert hl ((Leaf (c,i)), i) ) pql

{-tableToPQ t
 PRE: t maps each key to a positive value
 POST: takes each element in a Table Char Int and puts it into a PriorityQueue Char
 -}
tableToPQ :: Table Char Int -> PriorityQueue Char
tableToPQ t = Table.iterate t (PriorityQueue.insert) (PriorityQueue.empty)


{- codeTable h
   PURPOSE: Takes a HuffmanTree and gives a Table Char BitCode so it can be used to genrate a BitCode
   PRE:  True
   POST: a table that maps each character in h to its Huffman code
   EXAMPLES: codeTable Node (Leaf ('a',1)) 2 (Leaf ('b',1)) = T [(a,[True]), (b,[False])]
 -}

codeTable :: HuffmanTree -> Table Char BitCode
codeTable h = codeTableAux (huffmanTreeToBitCode h []) (Table.empty)

{-codeTableAux bl tb
  PRE: tb is empty
  POST: Takes a list of (Char,BitCode) and inserts them into a Table
-}
codeTableAux :: [(Char, BitCode)] -> Table Char BitCode -> Table Char BitCode
codeTableAux []     t = t
codeTableAux (x:xs) t = codeTableAux xs (Table.insert t (fst x) (snd x))

{-huffmanTreeToBitCode h b
  PRE: b is empty
  POST: given a HuffmanTree it returns a list of (Char,BitCode) so that all the elements in h are given the corect BitCode
-}
--VARIANT: the depth of the HuffmanTree
huffmanTreeToBitCode :: HuffmanTree -> BitCode -> [(Char,BitCode)]
huffmanTreeToBitCode (Leaf (c,_))   bcl = [(c,bcl)]
huffmanTreeToBitCode (Node lh _ rh) bcl = (huffmanTreeToBitCode lh (bcl ++ [False])) ++ (huffmanTreeToBitCode rh (bcl ++ [True]))


{- compress s
   PURPOSE: compresses a strung into a HuffmanTree and BitCode
   PRE:  True
   POST: (a Huffman tree based on s, the Huffman coding of s under this tree)
   EXAMPLES: compress "hello" = (Node (Node (Leaf ('h',1)) 2 (Leaf ('e',1))) 5 (Node (Leaf ('o',1)) 3 (Leaf ('l',2))),[False,False,False,True,True,True,True,True,True,False])
             compress "world" = (Node (Node (Leaf ('r',1)) 2 (Leaf ('l',1))) 5 (Node (Leaf ('d',1)) 3 (Node (Leaf ('w',1)) 2 (Leaf ('o',1)))),[True,True,False,True,True,True,False,False,False,True,True,False])
 -}
compress :: String -> (HuffmanTree, BitCode)
compress s =
  let
    h = huffmanTree (characterCounts s)
  in
  (h, (buildBitCode s (codeTable h)) )

{-buildBitCode s tb
  PRE: tb is the orespondion  Table Char BitCode of s
  POST: given a String and a coresponding Table Char BitCode it will return the bitcode for s
-}
--VATIANT: the length of s
buildBitCode :: String -> Table Char BitCode -> BitCode
buildBitCode []     t = []
buildBitCode (x:xs) t = (fromJust (Table.lookup t x) ) ++ buildBitCode xs t

{-fromJust m
  PRE: True
  POST: removes the maybe from a and is a is nothing error
-}
fromJust :: Maybe a -> a
fromJust Nothing  = error "called fromJust with Nothing"
fromJust (Just a) = a

{- decompress h bits
   PURPOSE: given a HuffmanTree and its BitCode this funktion will return the String that they corespond to
   PRE:  bits is a concatenation of valid Huffman code words for h
   POST: the decoding of bits under h
   EXAMPLES: ...
 -}
--VARIANT: the length of bits
decompress :: HuffmanTree -> BitCode -> String
decompress (Leaf (c,i)) bits = (replicate i c)
decompress h bits = reverse (decompressAux h bits [])

{-decompressAux h b s
  PRE:  s is empty, h and b are coresponding
  POST: will return the s that h and b are made from but in reverse
-}
decompressAux :: HuffmanTree -> BitCode -> String -> String
decompressAux _ []   s = s
decompressAux h bits s =
  let
     x = (getCharInt h bits 0)
  in decompressAux h (drop (snd x) bits) ((fst x):s)

{-getCharInt h b i
  PRE:  h and b are coresponding to the same String
  POST: given a HuffmanTree and BitCode it will return the first Char and how manny buts of the BitCode it "used" to represent it
-}
--VARIANT: The legth of BitCode
getCharInt :: HuffmanTree -> BitCode -> Int -> (Char, Int)
getCharInt (Leaf (c,_)) _ n = (c,n)
getCharInt (Node (lh) _ (rh) ) ((True):bits)  n = getCharInt rh bits (n+1)
getCharInt (Node (lh) _ (rh) ) ((False):bits) n = getCharInt lh bits (n+1)


--------------------------------------------------------------------------------
-- Test Cases
-- You may add your own test cases here:
-- Follow the pattern and/or read about HUnit on the interwebs.
--------------------------------------------------------------------------------

-- characterCounts
test1 = TestCase $ assertEqual "characterCounts"
            (Just 7) (Table.lookup (characterCounts "this is an example of a huffman tree") ' ')

-- codeTable
-- while the precise code for ' ' may vary, its length (for the given example string) should always be 3 bits
test2 = TestCase $ assertEqual "codeTable"
            3 (maybe (-1) length (Table.lookup (codeTable (huffmanTree (characterCounts "this is an example of a huffman tree"))) ' '))

-- compress
-- while the precise code for the given example string may vary, its length should always be 135 bits
test3 = TestCase $ assertEqual "compress"
            135 (length (snd (compress "this is an example of a huffman tree")))

-- decompress
test4 =
  let s = "this is an example of a huffman tree"
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

test5 =
    let s = "xxx"
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

test6 =
    let s = ""
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

-- for running all the tests
runtests = runTestTT $ TestList [test1, test2, test3, test4, test5, test6]
