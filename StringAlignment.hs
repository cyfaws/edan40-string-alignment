module StringAlignment where
import Data.List
import Data.Char

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

type AlignmentType = (String,String)

score :: Char -> Char -> Int
score '-' _ = scoreSpace
score _ '-' = scoreSpace
score x y 
   | x == y = scoreMatch
   | otherwise = scoreMismatch

similarityScore :: String -> String -> Int
similarityScore [] ys = length ys * scoreSpace
similarityScore xs [] = length xs * scoreSpace
similarityScore (x:xs) (y:ys) =
   maximum
   [ (similarityScore xs ys) + (score x y),
   (similarityScore xs (y:ys)) + (score x '-'),
   (similarityScore (x:xs) ys) + (score '-' y) ]

attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

maximaBy :: Ord b => (a -> b) -> [a] -> [a] 
maximaBy valueFcn xs = 
   let vs = map valueFcn xs; m = maximum vs
   in filter (\e -> m == (valueFcn e)) xs
   

optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] = [([], [])]
optAlignments (x:xs) [] = attachHeads x '-' ( optAlignments xs [] )
optAlignments [] (y:ys) = attachHeads '-' y ( optAlignments [] ys )
optAlignments (x:xs) (y:ys) = maximaBy (uncurry similarityScore) ( -- uncurry: (a -> b -> c) => ((a, b) -> c)
      attachHeads '-' y (optAlignments (x:xs) ys) ++ 
      attachHeads x '-' (optAlignments xs (y:ys)) ++ 
      attachHeads x y (optAlignments xs ys)
    )

outputOptAlignments :: String -> String -> IO ()
outputOptAlignments string1 string2 = do
   let alignments = optAlignments string1 string2
   putStrLn ("There are " ++ show (length alignments) ++ " optimal alignments: \n")
   mapM_ ( putStrLn.format ) alignments
      where
         format (a, b) = "\n" ++ spaces a ++ "\n" ++ spaces b
         spaces :: String -> String
         spaces [] = []
         spaces (x:xs) = x : ' ' : spaces (xs)