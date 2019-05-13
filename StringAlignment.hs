-- StIL-IDs: 
--      an7821he-s
--      eg6164to-s
--
-- Which part of the code was the hardest?
--      "We would say the memoization-part in optAlignments2!"
-- Why?
--      "Because we hade to fight on many fronts; some of them being:
--          * Finding a suitable memo-table
--          * Building said table
--          * Getting your head arounf the paradigm-shift. Iterative programming have the edge of
--            being more verbose, while functional programming does the heavy lifting for you."
--
-- Which part of your code are you most proud of?
--      "We settled on optAlignments. We think it consists of easy cases which makes it highly
--      readable"
--
-- Which part of your code would you like to receive feedback for?
--      "Prefferably all of the functions, but if we have to choose we would say optAlignment2 as it has
--      some hacky bits."

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

-- Calculates the maximum alignment score for a pair of strings.
-- OBS! No alignment included
similarityScore :: String -> String -> Int
similarityScore [] ys = length ys * scoreSpace
similarityScore xs [] = length xs * scoreSpace
similarityScore (x:xs) (y:ys) =
   maximum
   [ similarityScore xs ys      +   score x y,
     similarityScore xs (y:ys)  +   score x '-',
     similarityScore (x:xs) ys  +   score '-' y ]

-- Finds the maximum alignment score of 2 strings with the help of memoization. 
-- OBS! No alignment included
similarityScore2 :: String -> String -> Int
similarityScore2 xs ys = table !! (length xs) !! (length ys)
   where
   table = [[ entry i j | j<-[0..]] | i<-[0..]]
   
   entry :: Int -> Int -> Int
   entry 0 0 = 0
   entry i 0 = i * scoreSpace
   entry 0 j = j * scoreSpace
   entry i j = maximum
      [ (table  !!(i-1)     !!(j-1) ) + (score x   y ),
        (table  !!i         !!(j-1) ) + (score '-' y ),
        (table  !!(i-1)     !!j     ) + (score x '-' ) ]
           where
           x = xs!!(i-1)
           y = ys!!(j-1)

-- attaches 2 elements to a tuple of lists.
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

-- attaches 2 elements to a tuple of lists.
attachTails :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachTails t1 t2 aList = [((addTail t1 xs), (addTail t2 ys)) | (xs,ys) <- aList]
    where addTail t = reverse . (t:) . reverse

-- Gets the sublist of a list of the lists maximum (according to a function) values.
maximaBy :: Ord b => (a -> b) -> [a] -> [a] 
maximaBy valueFcn xs = 
   let vs = map valueFcn xs; m = maximum vs
   in filter (\e -> m == (valueFcn e)) xs
   
-- Finds the optimal alignment of 2 strings.
optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] = [([], [])]
optAlignments (x:xs) [] = attachHeads x '-' ( optAlignments xs [] )
optAlignments [] (y:ys) = attachHeads '-' y ( optAlignments [] ys )
optAlignments (x:xs) (y:ys) = maximaBy (uncurry similarityScore) ( 
      attachHeads '-' y (optAlignments (x:xs) ys) ++ 
      attachHeads x '-' (optAlignments xs (y:ys)) ++ 
      attachHeads x y (optAlignments xs ys)
    )

-- Pretty printing.
outputOptAlignments :: String -> String -> IO ()
outputOptAlignments string1 string2 = do
   let alignments = optAlignments2 string1 string2
   putStrLn ("There are " ++ show (length alignments) ++ " optimal alignments: \n")
   mapM_ ( putStrLn.format ) alignments
      where
         format (a, b) = "\n" ++ spaces a ++ "\n" ++ spaces b
         spaces :: String -> String
         spaces [] = []
         spaces (x:xs) = x : ' ' : spaces xs


-- Finds the optimal alignment of 2 strings, with a little help from memoization.
optAlignments2 :: String -> String -> [AlignmentType]
optAlignments2 xs ys = snd $ optAlig (length xs) (length ys)
  where
    optAlig i j = optTable!!i!!j
    optTable = [[ entry i j | j<-[0..]] | i<-[0..] ]
       
    entry :: Int -> Int -> (Int, [AlignmentType])
    entry 0 0 = (0, [("","")])
    entry i 0 = modEntry x '-' (optAlig (i-1) 0)
      where
         x = xs!!(i-1)
    entry 0 j = modEntry '-' y (optAlig 0 (j-1))
      where
         y = ys!!(j-1)

    entry i j = (fst (head bestAlignments), concatMap snd bestAlignments)
       where 
         x = xs!!(i-1)
         y = ys!!(j-1)
         bestAlignments = maximaBy fst [ (modEntry '-' y (optAlig i (j-1))),
                                         (modEntry x '-' (optAlig (i-1) j)),
                                         (modEntry x y (optAlig (i-1) (j-1)))]

-- Helper function; changes an entry {(Int, [AlignmentType])} as seen above.
modEntry t1 t2 (alscore, alig) = (alscore + (score t1 t2), attachTails t1 t2 alig)

