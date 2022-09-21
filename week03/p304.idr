import Data.Primitives.Views

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'

every_other : Stream a -> Stream a
every_other (_ :: (x :: xs)) = x :: (every_other xs)

square_root_approx : (number : Double) -> (approx : Double) -> Stream Double
square_root_approx number approx = approx :: square_root_approx number ((approx + (number / approx)) / 2)

square_root_bound : (max : Nat) -> (number : Double) -> (bound : Double) ->
  (approxs : Stream Double) -> Double
square_root_bound Z number bound (val :: xs) = val
square_root_bound (S k) number bound (val :: xs) =
  case abs((val * val) - number) < bound of
    True => val
    False => square_root_bound k number bound xs

square_root : (number : Double) -> Double
square_root number = square_root_bound 100 number 0.00000000001 (square_root_approx number number)

data Face = Heads | Tails

coinFlips : (count : Nat) -> Stream Int -> List Face
coinFlips count (value :: xs) with (divides value 2)
  coinFlips Z (value :: xs) | divBy = []
  coinFlips (S k) ((_ + 0) :: xs) | (DivBy prf) = Heads :: coinFlips k xs
  coinFlips (S k) ((_ + 1) :: xs) | (DivBy prf) = Tails :: coinFlips k xs
