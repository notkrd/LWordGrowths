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
