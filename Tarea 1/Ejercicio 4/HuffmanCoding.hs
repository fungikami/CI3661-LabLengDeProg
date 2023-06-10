-- Implementation of Huffman Coding Algorithm

module HuffmanCoding where

import Data.List as List (sortBy, insertBy)
import Data.Map as Map (fromListWith, toList)

-- Data type for Huffman Tree
-- Leaf contains the frequency and the character
-- Node contains the frequency, the list of characters, and the left and right subtrees
data Tree = Leaf Int Char
            | Node Int [Char] Tree Tree
            deriving (Show, Eq)

-- Get the frequency of a node 
getFreq :: Tree -> Int
getFreq (Leaf f _) = f
getFreq (Node f _ _ _) = f

-- Get the characters of a node
getChars :: Tree -> [Char]
getChars (Leaf _ c) = [c]
getChars (Node _ cs _ _) = cs

-- Create map of frequencies: occurrences 
freqMap :: String -> [(Char, Int)]
freqMap s = Map.toList (Map.fromListWith (+) [(c, 1) | c <- s])

-- Sort map of frequencies
sortFreqMap :: [(Char, Int)] -> [(Char, Int)]
sortFreqMap = List.sortBy (\(_,a) (_,b) -> compare a b)

-- Create Huffman Tree
createTree :: String -> Tree
createTree s =  let freqs = sortFreqMap (freqMap s)
                    trees = map (\(c, f) -> Leaf f c) freqs
                in createTree' trees

-- Create Huffman Tree Auxiliar: 
createTree' :: [Tree] -> Tree
createTree' [] = Leaf 0 ' '             -- Empty node
createTree' [t] = t                     -- Single node
createTree' (t1:t2:ts) =                -- Two or more nodes
    let t' = Node (getFreq t1 + getFreq t2) (getChars t1 ++ getChars t2) t1 t2
    in createTree' (insertTree (t':ts))

-- Insert tree
insertTree :: [Tree] -> [Tree]
insertTree [] = []
insertTree [t] = [t]
insertTree t = sortBy (\t1 t2 -> compare (getFreq t1) (getFreq t2)) t

-------------------- Encoding / Decoding --------------------

-- Generate binary coding for each character
generateBinary :: Tree -> [([Char], Char)]
generateBinary (Leaf _ c) = [("1", c)]     -- Single node, path is 1
generateBinary (Node _ _ l r) = generateBinary' l "0" ++ generateBinary' r "1"

-- Auxiliar function to generate binary coding for each character
generateBinary' :: Tree -> [Char] -> [([Char], Char)]
generateBinary' (Leaf _ c) s = [(s, c)]
generateBinary' (Node _ _ l r) s = generateBinary' l (s ++ "0") ++ generateBinary' r (s ++ "1")

-- 
searchByChar :: Eq val => [(key, val)] -> val -> Maybe key 
searchByChar [] _ = Nothing
searchByChar ((k, v) : s) lookVal   | v == lookVal = Just k 
                                    | otherwise = searchByChar s lookVal

-- Encode
encode :: String -> (String, Tree)
encode string = 
    let hTree = createTree string
        table = generateBinary hTree
    in encodeHelper string table hTree
    where
        encodeHelper [] _ hTree = ("", hTree)
        encodeHelper (c:cs) table hTree =   
            let char = searchByChar table c
            in case char of
                Nothing -> ("", hTree)
                Just char -> let (rest, tree) = encodeHelper cs table hTree
                            in (char ++ rest, tree)

-- Decode
decode :: (String, Tree) -> String
decode (msg, tree) =    let table = generateBinary tree
                        in decodeHelper msg table
                        where
                            decodeHelper [] _ = ""
                            decodeHelper msg table = decode' msg table 1 ""

decode' :: String -> [([Char], Char)] -> Int -> String -> String
decode' [] table i [] = "" 
decode' [] table i decodedMsg = decodedMsg 
decode' msg table i decodedMsg = 
    let org_text = lookup (take i msg) table
    in case org_text of
        Nothing -> decode' msg table (i + 1) decodedMsg
        Just decodedChar ->
            decode' (drop i msg) table 1 (decodedMsg ++ [decodedChar])

-- Main
-- main :: IO ()
-- main = do
--     input <- getLine
--     let (encoded, resultTree) = encode input
--     let decoded = decode (encoded, resultTree)
--     putStrLn $ "Encoded message: " ++ encoded
--     putStrLn $ "Decoded message: " ++ decoded
--     return ()
