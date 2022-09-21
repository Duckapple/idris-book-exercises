{- 1 -}

same_cons : {xs : List a} -> {ys : List a} ->
  xs = ys -> x :: xs = x :: ys
same_cons prf = cong prf

{- 2 -}

same_lists : {xs : List a} -> {ys : List a} ->
  x = y -> xs = ys -> x :: xs = y :: ys
same_lists Refl prf = cong prf

{- 3 -}

data ThreeEq : {t:Type} -> (a:t) -> (b:t) -> (c:t) -> Type where
  ThreeRefl : (x:t) -> ThreeEq x x x

{- 4 -}

allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS z z z (ThreeRefl z) = ThreeRefl (S z)

