{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
module War (deal) where

import Data.List ( sort )

{--
Function stub(s) with type signatures for you to fill in are given below. 
Feel free to add as many additional helper functions as you want. 

The tests for these functions can be found in src/TestSuite.hs. 
You are encouraged to add your own tests in addition to those provided.

Run the tester by executing 'cabal test' from the war directory 
(the one containing war.cabal)
--}

sortDescending :: Ord a => [a] -> [a]
sortDescending = reverse . sort

deal :: [Int] -> [Int]
deal shuf =
    let p1 = reverse $ fst $ split shuf
        p2 = reverse $ snd $ split shuf
        holder = fight p1 p2 []
        winner = map replaceAces holder
  in winner

split :: [Int] -> ([Int], [Int])
split deck =
  let p1 = [replaceOnes (deck !! i) | i <- [0,2..length deck - 1]]
      p2 = [replaceOnes (deck !! i) | i <- [1,3..length deck - 1]]
  in (p1, p2)

replaceOnes :: Int -> Int
replaceOnes value =
  if value == 1
    then 14
    else value

replaceAces :: Int -> Int
replaceAces value =
  if value == 14
    then 1
    else value

fight :: [Int] -> [Int] -> [Int] -> [Int]
fight startp1 startp2 tie =
  if null startp1 || null startp2
    then
      if null startp1
        then startp2 ++ sortDescending tie
        else startp1 ++ sortDescending tie
  else
    let h1:t1 = startp1
        h2:t2 = startp2
    in if h1 > h2
        then fight (t1 ++ sortDescending (tie ++ [h1, h2])) t2 []
        else if h2 > h1
          then fight t1 (t2 ++ sortDescending (tie ++ [h2, h1])) []
          else if length startp1 == 2
            then let h2:w2:t2' = startp2
                 in fight [] t2' (tie ++ [h2, w2] ++ startp1)
            else if length startp2 == 2
              then let h1:w1:t1' = startp1
                   in fight t1' [] (tie ++ [h1, w1] ++ startp2)
              else let h1:w1:t1' = startp1
                       h2:w2:t2' = startp2
                   in fight t1' t2' (tie ++ [h1, h2, w1, w2])