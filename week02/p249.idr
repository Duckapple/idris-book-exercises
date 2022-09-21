{- 2 -}

data Last : List a -> a -> Type where
     LastOne : Last [value] value
     LastCons : (prf : Last xs value) -> Last (x :: xs) value

notInNil : Last [] value -> Void
notInNil x impossible

notLast : (contra : x = y -> Void) -> Last [x] y -> Void
notLast contra LastOne = contra Refl
notLast contra (LastCons prf) impossible

notAfter : (contra : Last (y :: xs) value -> Void) -> Last (x :: (y :: xs)) value -> Void
notAfter contra (LastCons prf) = contra prf

isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No (notInNil)
isLast [x] value = case decEq x value of
     Yes Refl => Yes LastOne
     No contra => No (notLast contra)
isLast (x :: (y :: xs)) value = case isLast (y :: xs) value of
     Yes prf => Yes (LastCons prf)
     No contra => No (notAfter contra)

