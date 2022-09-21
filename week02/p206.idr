data Expr num = Val num
              | Add (Expr num) (Expr num)
              | Sub (Expr num) (Expr num)
              | Mul (Expr num) (Expr num)
              | Div (Expr num) (Expr num)
--              | Abs (Expr num)

eval : (Neg num, Integral num) => Expr num -> num
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
-- eval (Abs x) = abs (eval x)

Num ty => Num (Expr ty) where
    (+) = Add
    (*) = Mul
    fromInteger = Val . fromInteger

Neg ty => Neg (Expr ty) where
    negate x = 0 - x
    (-) = Sub
--    abs = Abs

data Vect : Nat -> Type -> Type where
     Nil : Vect Z a
     (::) : a -> Vect k a -> Vect (S k) a

Functor Expr where
  map func (Val x) = Val (func x)
  map func (Add x y) = (Add (map func x) (map func y))
  map func (Sub x y) = (Sub (map func x) (map func y))
  map func (Mul x y) = (Mul (map func x) (map func y))
  map func (Div x y) = (Div (map func x) (map func y))

Eq ty => Eq (Vect n ty) where
  (==) [] [] = True
  (==) (x :: xs) (y :: ys) = x == y && xs == ys
  (/=) [] [] = False
  (/=) (x :: xs) (y :: ys) = x /= y || xs /= ys

Foldable (Vect n) where
  foldr func init [] = init
  foldr func init (x :: xs) = func x (foldr func init xs)
  foldl func init [] = init
  foldl func init (x :: xs) = foldl func (func init x) xs
