import Data.Vect

data Shape : Type where
  Rectangle: (base : Double) -> (height : Double) -> Shape
  Triangle: (base : Double) -> (height : Double) -> Shape
  Circle: (radius : Double) -> Shape

area : Shape -> Double
area (Rectangle base height) = base * height
area (Triangle base height) = base * height / 2
area (Circle radius) = pi * radius * radius

Square : (side : Double) -> Shape
Square side = Rectangle side side

data PowerSource = Petrol | Pedal

data Vehicle : PowerSource -> Type where
  Bicycle : Vehicle Pedal
  Unicycle : Vehicle Pedal
  Car : (fuel : Nat) -> Vehicle Petrol
  Bus : (fuel : Nat) -> Vehicle Petrol
  Motorcycle : (fuel : Nat) -> Vehicle Petrol

wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels Unicycle = 1
wheels (Car fuel) = 4
wheels (Bus fuel) = 12
wheels (Motorcycle fuel) = 2

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car (fuel + 200)
refuel (Bus fuel) = Bus (fuel + 800)
refuel (Motorcycle fuel) = Motorcycle (fuel + 60)


data Vector2D : Type
  where
    Polar2D : (a : Double) -> (m : Double) -> Vector2D
    Cartesian2D : (x : Double) -> (y : Double) -> Vector2D

vectTake : (n : Nat) -> Vect (n + m) a -> Vect n a
vectTake Z xs = []
vectTake (S k) (x :: xs) = x :: vectTake k xs

pluus : Nat -> Nat -> Nat
pluus Z j = j
pluus (S k) j = S (pluus k j)

||| Keeps the type of the leftmost argument
plus : Vector2D -> Vector2D -> Vector2D
plus (Polar2D a1 m1) (Polar2D a2 m2) = (Polar2D ((a1 + a2) / 2) ?magnitude)
plus (Polar2D a1 m1) (Cartesian2D x2 y2) = ?wow_4
plus (Cartesian2D x z) y = ?wow_2

printLonger : IO ()
printLonger =
  do
    putStr "Input string 1: "
    input1 <- getLine
    putStr "Input string 2: "
    input2 <- getLine
    let l1 = length input1
    let l2 = length input2
    if l1 < l2
      then putStrLn(show l2)
      else putStrLn(show l1)

printLoonger : IO ()
printLoonger =
  putStr "Input string 1: " >>= (\_ => getLine) >>=
    (\l1 => (putStr "Input string 2: " >>= (\_ => getLine) >>= (\l2 => pure (length l1, length l2)))) >>=
      (\(len1, len2) =>
        if len1 < len2
          then putStrLn(show len2)
          else putStrLn(show len1))

-- Helper function for guess
readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
    then pure (Just (cast input))
    else pure Nothing

guess : (target : Nat) -> IO ()
guess target =
  do
    putStr "guess: "
    input <- readNumber
    case input of
      Nothing => do
        putStrLn "Wasn't a number, try again."
        guess target
      Just g => if g < target
        then do
          putStrLn "Too low."
          guess target
        else if target < g
          then do
            putStrLn "Too high."
            guess target
          else do
            putStrLn "Correct, you got it!"

