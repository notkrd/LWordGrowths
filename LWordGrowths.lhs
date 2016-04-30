We will play with using Lindenmayer systems to generate sentences.

\begin{code}

module LWordGrowths where

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

\end{code}

This hopes to be a minimal implementation of a deterministic Lindenmeyer system.

\begin{code}

data LSystem a = LSystem {l_start :: [a], l_rules :: [([a], [a])]} deriving (Eq)

applyRule :: (Eq a) => [a] -> Maybe [a] -> (Bool, [a])
applyRule some_str Nothing = (False, some_str)
applyRule some_str (Just the_remainder) = (True, the_remainder)

expandSysWithRules :: (Eq a) => [([a], [a])] -> [([a], [a])] -> [a] -> [a]
expandSysWithRules _ _ [] = []
expandSysWithRules all_rules [] (fst_symb : rest_symbs) =
  fst_symb : expandSysWithRules all_rules all_rules rest_symbs
expandSysWithRules all_rules (fst_rule : other_rules) some_str
  | it_worked == True = (snd fst_rule) ++ (expandSysWithRules all_rules all_rules the_rest)
  | otherwise = expandSysWithRules all_rules other_rules some_str
  where (it_worked, the_rest) = applyRule some_str (stripPrefix (fst fst_rule) some_str)

rewriteLSystem :: (Eq a) => LSystem a -> LSystem a
rewriteLSystem an_l_system =
  an_l_system {l_start = expandSysWithRules (l_rules an_l_system) (l_rules an_l_system) (l_start an_l_system)}

nthGrowthOf :: (Eq a) => Int -> LSystem a -> LSystem a
nthGrowthOf n an_lsystem = (iterate rewriteLSystem an_lsystem) !! n

\end{code}

Tests:

\begin{code}

fibAlg :: LSystem Char
fibAlg = LSystem "a" [("a","ab"),("b","a")]

\end{code}

Experiments:

\begin{code}

writeSystemAfter :: Int -> LSystem String -> String
writeSystemAfter n an_l_system =
  concat (intersperse " " (l_start (nthGrowthOf n an_l_system)))

lang_starts :: [String]
lang_starts = ["language","starts"]

wordsMakeWords :: [([String], [String])]
wordsMakeWords = [
  (["in","an","old","house","i","have","forgotten","now"],["language"]),
  (["starts","somewhere"], ["starts","now","somewhere","in","something"]),
  (["cannot","remember"],["have","forgotten","the","interiors","of"]),
  (["the","interiors","of"],["now"]),
  (["old","odd"],["old"]),
  (["somewhere","in"], ["in", "some", "faraway"]),
  (["breath","now"],["breath"]),
  (["language","language"],["language","starts"]),
  (["this"],["in","some","odd","place"]),
  (["some"],["an","old"]),
  (["place"],["building"]),
  (["building"],["house","i","cannot","remember"]),
  (["faraway","something"],["faraway","breath"]),
  (["language"],["this","language"]),
  (["starts"],["starts","somewhere"])]

langGrows :: LSystem String
langGrows = LSystem lang_starts wordsMakeWords

writeGrowthOf :: Int -> LSystem String -> String
writeGrowthOf n an_l_system
  | n == 0 = concat (intersperse " " (l_start an_l_system))
  | n > 0 = (concat (intersperse " " (l_start an_l_system))) ++
    ('\n' : '\n' : (writeGrowthOf (n - 1) (rewriteLSystem an_l_system)))
    
doStuff :: IO ()
doStuff = putStrLn (writeGrowthOf 12 langGrows)

\end{code}

