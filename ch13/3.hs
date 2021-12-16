{-
3.  本誌の数式の3つ目の構文規則を用いて「2+3」「2*3*4」「(2+3)+4」に対する構文木を書いてください
-}

{-
それぞれ最右導出の系列を記す
-- 2+3
expr  --> term + expr --> term + factor --> term + nat --> term + 3
      --> factor + 3 --> nat + 3 --> 2 + 3


-- 2*3*4
expr --> term --> factor * term --> factor * factor * term --> factor * factor * factor 
  --> factor * factor * nat --> factor * factor * 4 --> factor * nat * 4 --> factor * 3 * 4
  --> nat * 3 * 4 --> 2 * 3 * 4

-- (2+3)+4
expr --> term + expr --> term + term --> term + factor --> term + nat --> term + 4
  --> factor + 4 --> (expr) + 4 --> (term + expr) + 4 --> (term + term) + 4
  --> (term + factor) + 4 --> (term + nat) + 4 --> (term + 3) + 4 
  --> (factor + 3) + 4 --> (nat + 3) + 4 --> (2 + 3) + 4
-}

