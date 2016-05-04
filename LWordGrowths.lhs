We will play with using Lindenmayer systems to generate sentences.

\begin{code}

module LWordGrowths where

import Data.List

\end{code}

This hopes to be a minimal implementation of a deterministic Lindenmeyer system.

\begin{code}

data LSystem a = LSystem {l_start :: [a], l_rules :: [([a], [a])]} deriving (Eq)

applyIfWorked :: (Eq a) => [a] -> Maybe [a] -> (Bool, [a])
applyIfWorked some_str Nothing = (False, some_str)
applyIfWorked some_str (Just the_remainder) = (True, the_remainder)

expandSysWithRules :: (Eq a) => [([a], [a])] -> [([a], [a])] -> [a] -> [a]
expandSysWithRules _ _ [] = []
expandSysWithRules all_rules [] (fst_symb : rest_symbs) =
  fst_symb : expandSysWithRules all_rules all_rules rest_symbs
expandSysWithRules all_rules (fst_rule : other_rules) some_str
  | (fst fst_rule) == [] = expandSysWithRules all_rules other_rules some_str
  | it_worked == True = (snd fst_rule) ++ (expandSysWithRules all_rules all_rules the_rest)
  | otherwise = expandSysWithRules all_rules other_rules some_str
  where (it_worked, the_rest) = applyIfWorked some_str (stripPrefix (fst fst_rule) some_str)

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
  (["something","now","in","an"],["breath","in","an"]),
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
  (["language"],["this","language"]),
  (["starts"],["starts","somewhere"])]

langGrows :: LSystem String
langGrows = LSystem lang_starts wordsMakeWords

sitting_starts :: [String]
sitting_starts = ["sitting"]

sittingIntoWords :: [([String],[String])]
sittingIntoWords = [
  (["sitting"],["sitting","here"]),
  (["here"],["on","wood"]),
  (["wood"],["this","hard","surface"]),
  (["hard"],["smooth","and","hard"]),
  (["smooth"],["quiet","and","obscure"])]

sittingSystem = LSystem sitting_starts sittingIntoWords


sentence_starts :: [String]
sentence_starts = ["sentence"]

sentence_grows :: [([String],[String])]
sentence_grows = [
  (["sentence"],["noun phrase [1]","verb phrase"]),
  (["noun phrase [1]"],["determiner [1]","noun"]),
  (["noun phrase [2]"],["determiner [2]","noun [2]"]),
  (["verb phrase"],["verb (transitive)","noun phrase [2]"]),
  (["verb (transitive)"],["verb (transitive) [1]","and","verb (transitive) [2]"]),
  (["noun"],["adjective [0]","noun [1]"]),
  (["noun [1]"],["adjective [1]","noun [3]"]),
  (["noun [2]"],["adjective [2]","noun [4]"]),
  (["noun [4]"],["adjective [4]","noun [4]"]),
  (["noun [3]"],["grammarian"]),
  (["adjective [0]"],["slithy"]),
  (["adjective [1]"],["platitudinous"]),
  (["adjective [2]"],["obscure"]),
  (["adjective [4]"],["silly"]),
  (["determiner [1]"],["the"]),
  (["determiner [2]"],["an"]),
  (["verb (transitive) [1]"],["suggests"]),
  (["verb (transitive) [2]"],["masticates"])]

sentenceSystem = LSystem sentence_starts sentence_grows

talking_death :: [([String],[String])]
talking_death = [
  (["just","die"],["get","dead"]),
  (["just","get"],["become"]),
  (["just","become"],["die","and","be"]),
  (["death"],["i","could","die"]),
  (["could"],["could","just"])]

talkingDeath :: LSystem String
talkingDeath = LSystem ["death"] talking_death

thinking_death :: [([String],[String])]
thinking_death = [
  (["just","die"],["get","dead"]),
  (["just","get"],["become"]),
  (["just","become"],["die","or","be"]),
  (["dead"],["thinking","about","being","dead"]),
  (["death"],["i","could","die"]),
  (["could"],["could","just"])]

thinkingDeath :: LSystem String
thinkingDeath = LSystem ["death"] thinking_death

writeGrowthOf :: Int -> LSystem String -> String
writeGrowthOf n an_l_system
  | n == 0 = concat (intersperse " " (l_start an_l_system))
  | n > 0 = (concat (intersperse " " (l_start an_l_system))) ++
    ('\n' : '\n' : (writeGrowthOf (n - 1) (rewriteLSystem an_l_system)))
    
doStuff :: IO ()
doStuff = putStrLn (writeGrowthOf 10 langGrows)

badStuff :: IO ()
badStuff = putStrLn (writeGrowthOf 16 talkingDeath)

anyStuff :: Int -> LSystem String -> IO ()
anyStuff n some_system = putStrLn (writeGrowthOf n some_system)

\end{code}

