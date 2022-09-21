module Main

import Data.Vect

palindrome : Nat -> String -> Bool
palindrome n x = (length x >= n) && reverse (toLower x) == toLower x

counts : String -> (Nat, Nat)
counts x = (length (words x), length x)

top_ten : Ord a => List a -> List a
top_ten xs = (take 10 (reverse (sort xs)))

my_length : List a -> Nat
my_length [] = 0
my_length (x :: xs) = 1 + my_length xs

my_reverse : List a -> List a
my_reverse [] = []
my_reverse (x :: xs) = my_reverse xs ++ [x]

my_map : (a -> b) -> List a -> List b
my_map f [] = []
my_map f (x :: xs) = f x :: my_map f xs

my_vect_map : (a -> b) -> Vect n a -> Vect n b
my_vect_map f [] = []
my_vect_map f (x :: xs) = f x :: my_vect_map f xs

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = replicate _ []
transposeMat (x :: xs) =
  let xsTransposed = transposeMat xs in
    zipWith (::) x xsTransposed

addMatrix : Num a => Vect n (Vect m a) ->  Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) =
  let res = addMatrix xs ys in
    zipWith (+) x y :: res

mulMatrix_line : Num a => (x : Vect m a) -> (ys : Vect p (Vect m a)) -> Vect p a
mulMatrix_line x [] = []
mulMatrix_line x (y :: ys) = sum(zipWith (*) x y) :: mulMatrix_line x ys

multMatrix_impl : Num a => Vect n (Vect m a) -> Vect p (Vect m a) -> Vect n (Vect p a)
multMatrix_impl [] ys = []
multMatrix_impl xs [] = replicate _ []
multMatrix_impl (x :: xs) ys = mulMatrix_line x ys :: multMatrix_impl xs ys

multMatrix : Num a => Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
multMatrix xs ys = multMatrix_impl xs (transposeMat ys)

double : Vect n a -> Vect (n * 2) a
double [] = []
double (x :: xs) = x :: x :: double xs

countTo_impl : (n : Nat) -> Vect n Nat
countTo_impl Z = []
countTo_impl (S k) = S k :: countTo_impl (k)

countTo : (m : Nat) -> Vect m Nat
countTo m = reverse (countTo_impl m)

takeList : (n : Nat) -> List a -> Maybe (Vect n a)
takeList Z xs = Just([])
takeList n [] = Nothing
takeList (S k) (x :: xs) = map (\vec => x :: vec) (takeList k xs)
