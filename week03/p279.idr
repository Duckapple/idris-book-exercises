import Data.List.Views
import Data.Vect.Views
import Data.Nat.Views

equalSuffix : Eq a => List a -> List a -> List a
equalSuffix xs ys with (snocList xs)
  equalSuffix [] ys | Empty = []
  equalSuffix (xs ++ [x]) ys | (Snoc recXs) with (snocList ys)
    equalSuffix (xs ++ [x]) [] | (Snoc recXs) | Empty = []
    equalSuffix (xs ++ [x]) (ys ++ [y]) | (Snoc recXs) | (Snoc rec) =
      case x == y of
        True => (equalSuffix xs ys) ++ [x]
        False => []

toBinary : Nat -> String
toBinary k with (halfRec k)
  toBinary Z | HalfRecZ = ""
  toBinary (n + n) | (HalfRecEven rec) = toBinary n ++ "0" | rec
  toBinary (S (n + n)) | (HalfRecOdd rec) = toBinary n ++ "1" | rec

palindrome : Eq a => List a -> Bool
palindrome xs with (vList xs)
  palindrome [] | VNil = True
  palindrome [x] | VOne = True
  palindrome (x :: (ys ++ [y])) | (VCons rec) =
    case x == y of
      True => palindrome ys | rec
      False => False
