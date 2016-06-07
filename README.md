# LWordGrowths
Generates ideally poetic sentences using Lindenmeyer systems. It's fairly minimal, but with the following set of rules generates the below texts:

wordsMakeWords = [
  (["in","an","old","house","i","have","forgotten","now"],["language"]),
  (["old","faraway","something"],["old","faraway","breath"]),
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

(Where we begin with "language starts", and successively replace strings of words in the beginnings of tuples with the strings in the corresponding ends)

1. language starts

2. this language starts somewhere

3. in some odd place this language starts now somewhere in something

4. in an old odd building in some odd place this language starts somewhere now in some faraway something

5. in an old house i cannot remember in an old odd building in some odd place this language starts now somewhere in something now in an old faraway something

6. in an old house i have forgotten the interiors of in an old house i cannot remember in an old odd building in some odd place this language starts somewhere now in some faraway something now in an old faraway breath

7. in an old house i have forgotten now in an old house i have forgotten the interiors of in an old house i cannot remember in an old odd building in some odd place this language starts now somewhere in something now in an old faraway something now in an old faraway breath

8. language in an old house i have forgotten now in an old house i have forgotten the interiors of in an old house i cannot remember in an old odd building in some odd place this language starts somewhere now in some faraway something now in an old faraway breath now in an old faraway breath

9. this language language in an old house i have forgotten now in an old house i have forgotten the interiors of in an old house i cannot remember in an old odd building in some odd place this language starts now somewhere in something now in an old faraway something now in an old faraway breath in an old faraway breath

10. in some odd place language starts language in an old house i have forgotten now in an old house i have forgotten the interiors of in an old house i cannot remember in an old odd building in some odd place this language starts somewhere now in some faraway something now in an old faraway breath now in an old faraway breath in an old faraway breath

11. in an old odd building this language starts somewhere this language language in an old house i have forgotten now in an old house i have forgotten the interiors of in an old house i cannot remember in an old odd building in some odd place this language starts now somewhere in something now in an old faraway something now in an old faraway breath in an old faraway breath in an old faraway breath

12. in an old house i cannot remember in some odd place this language starts now somewhere in something in some odd place language starts language in an old house i have forgotten now in an old house i have forgotten the interiors of in an old house i cannot remember in an old odd building in some odd place this language starts somewhere now in some faraway something now in an old faraway breath now in an old faraway breath in an old faraway breath in an old faraway breath

13. in an old house i have forgotten the interiors of in an old odd building in some odd place this language starts somewhere now in some faraway something in an old odd building this language starts somewhere this language language in an old house i have forgotten now in an old house i have forgotten the interiors of in an old house i cannot remember in an old odd building in some odd place this language starts now somewhere in something now in an old faraway something now in an old faraway breath in an old faraway breath in an old faraway breath in an old faraway breath

14. in an old house i have forgotten now in an old house i cannot remember in an old odd building in some odd place this language starts now somewhere in something now in an old faraway something in an old house i cannot remember in some odd place this language starts now somewhere in something in some odd place language starts language in an old house i have forgotten now in an old house i have forgotten the interiors of in an old house i cannot remember in an old odd building in some odd place this language starts somewhere now in some faraway something now in an old faraway breath now in an old faraway breath in an old faraway breath in an old faraway breath in an old faraway breath

15. language in an old house i have forgotten the interiors of in an old house i cannot remember in an old odd building in some odd place this language starts somewhere now in some faraway something now in an old faraway breath in an old house i have forgotten the interiors of in an old odd building in some odd place this language starts somewhere now in some faraway something in an old odd building this language starts somewhere this language language in an old house i have forgotten now in an old house i have forgotten the interiors of in an old house i cannot remember in an old odd building in some odd place this language starts now somewhere in something now in an old faraway something now in an old faraway breath in an old faraway breath in an old faraway breath in an old faraway breath in an old faraway breath

16. this language in an old house i have forgotten now in an old house i have forgotten the interiors of in an old house i cannot remember in an old odd building in some odd place this language starts now somewhere in something now in an old faraway something now in an old faraway breath in an old house i have forgotten now in an old house i cannot remember in an old odd building in some odd place this language starts now somewhere in something now in an old faraway something in an old house i cannot remember in some odd place this language starts now somewhere in something in some odd place language starts language in an old house i have forgotten now in an old house i have forgotten the interiors of in an old house i cannot remember in an old odd building in some odd place this language starts somewhere now in some faraway something now in an old faraway breath now in an old faraway breath in an old faraway breath in an old faraway breath in an old faraway breath in an old faraway breath


[
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

1. sentence

2. noun phrase [1] verb phrase

3. determiner [1] noun verb (transitive) noun phrase [2]

4. the adjective [0] noun [1] verb (transitive) [1] and verb (transitive) [2] determiner [2] noun [2]

5. the slithy adjective [1] noun [3] suggests and masticates an adjective [2] noun [4]

6. the slithy platitudinous grammarian suggests and masticates an obscure adjective [4] noun [4]

7. the slithy platitudinous grammarian suggests and masticates an obscure silly adjective [4] noun [4]

8. the slithy platitudinous grammarian suggests and masticates an obscure silly silly adjective [4] noun [4]

[
  (["just","die"],["get","dead"]),
  (["just","get"],["become"]),
  (["just","become"],["die","or","be"]),
  (["dead"],["thinking","about","being","dead"]),
  (["death"],["i","could","die"]),
  (["could"],["could","just"])]

1. death

2. i could die

3. i could just die

4. i could just get dead

5. i could just become thinking about being dead

6. i could just die or be thinking about being thinking about being dead

7. i could just get dead or be thinking about being thinking about being thinking about being dead

8. i could just become thinking about being dead or be thinking about being thinking about being thinking about being thinking about being dead

9. i could just die or be thinking about being thinking about being dead or be thinking about being thinking about being thinking about being thinking about being thinking about being dead

10. i could just get dead or be thinking about being thinking about being thinking about being dead or be thinking about being thinking about being thinking about being thinking about being thinking about being thinking about being dead

11. i could just become thinking about being dead or be thinking about being thinking about being thinking about being thinking about being dead or be thinking about being thinking about being thinking about being thinking about being thinking about being thinking about being thinking about being dead

[
  (["sitting"],["sitting","here"]),
  (["here"],["on","wood"]),
  (["wood"],["this","hard","surface"]),
  (["hard"],["smooth","and","firm"]),
  (["smooth"],["quiet","and","obscure"]),
  (["surface"],["surface","thinking"]),
  (["thinking"],["making","language","and"]),
  (["making"],["going","about","or","finding"]),
  (["language"],["words","for","nothing"]),
  (["nothing"],["no","thing","at","all"])]

1. sitting

2. sitting here

3. sitting here on wood

4. sitting here on wood on this hard surface

5. sitting here on wood on this hard surface on this smooth and firm surface thinking

6. sitting here on wood on this hard surface on this smooth and firm surface thinking on this quiet and obscure and firm surface thinking making language and

7. sitting here on wood on this hard surface on this smooth and firm surface thinking on this quiet and obscure and firm surface thinking making language and on this quiet and obscure and firm surface thinking making language and going about or finding words for nothing and

8. sitting here on wood on this hard surface on this smooth and firm surface thinking on this quiet and obscure and firm surface thinking making language and on this quiet and obscure and firm surface thinking making language and going about or finding words for nothing and on this quiet and obscure and firm surface thinking making language and going about or finding words for nothing and going about or finding words for no thing at all and

9. sitting here on wood on this hard surface on this smooth and firm surface thinking on this quiet and obscure and firm surface thinking making language and on this quiet and obscure and firm surface thinking making language and going about or finding words for nothing and on this quiet and obscure and firm surface thinking making language and going about or finding words for nothing and going about or finding words for no thing at all and on this quiet and obscure and firm surface thinking making language and going about or finding words for nothing and going about or finding words for no thing at all and going about or finding words for no thing at all and

