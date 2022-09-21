import Data.Vect

data StackCmd : Type -> Nat -> Nat -> Type where
     Push : Integer -> StackCmd () height (S height)
     Pop : StackCmd Integer (S height) height
     Top : StackCmd Integer (S height) (S height)

     GetStr : StackCmd String height height
     PutStr : String -> StackCmd () height height

     Pure : ty -> StackCmd ty height height
     (>>=) : StackCmd a height1 height2 ->
             (a -> StackCmd b height2 height3) ->
             StackCmd b height1 height3




runStack : (stk : Vect inHeight Integer) ->
           StackCmd ty inHeight outHeight -> IO (ty, Vect outHeight Integer)
runStack stk (Push val) = pure ((), val :: stk)
runStack (val :: stk) Pop = pure (val, stk)
runStack (val :: stk) Top = pure (val, val :: stk)
runStack stk GetStr = do x <- getLine
                         pure (x, stk)
runStack stk (PutStr x) = do putStr x
                             pure ((), stk)
runStack stk (Pure x) = pure (x, stk)
runStack stk (x >>= f) = do (x', newStk) <- runStack stk x
                            runStack newStk (f x')


data StackIO : Nat -> Type where
     Do : StackCmd a height1 height2 -> 
          (a -> Inf (StackIO height2)) -> StackIO height1

namespace StackDo
     (>>=) : StackCmd a height1 height2 -> 
             (a -> Inf (StackIO height2)) -> StackIO height1
     (>>=) = Do

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

run : Fuel -> Vect height Integer -> StackIO height -> IO ()
run (More fuel) stk (Do c f) 
     = do (res, newStk) <- runStack stk c
          run fuel newStk (f res)
run Dry stk p = pure ()

              
              

data StkInput = Number Integer
              | Add
              | Multiply
              | Subtract
              | Negate
              | Discard
              | Duplicate

strToInput : String -> Maybe StkInput
strToInput "" = Nothing 
strToInput "add" = Just Add
strToInput "subtract" = Just Subtract
strToInput "multiply" = Just Multiply
strToInput "negate" = Just Negate
strToInput "discard" = Just Discard
strToInput "duplicate" = Just Duplicate
strToInput x = if all isDigit (unpack x) 
                  then Just (Number (cast x))
                  else Nothing


doAdd : StackCmd () (S (S height)) (S height) 
doAdd = do val1 <- Pop
           val2 <- Pop
           Push (val1 + val2)

doSubtract : StackCmd () (S (S height)) (S height)
doSubtract = do
  v1 <- Pop
  v2 <- Pop
  Push $ v1 - v2

doMultiply : StackCmd () (S (S height)) (S height)
doMultiply = do
  v1 <- Pop
  v2 <- Pop
  Push $ v1 * v2

doNegate : StackCmd () (S height) (S height)
doNegate = do
  val <- Pop
  Push $ 0 - val

mutual
  binTry : ({h1 : Nat} -> StackCmd () (2 + h1) (S h1)) -> StackIO height
  binTry cmd {height = (S (S h))} = do
    cmd
    result <- Top
    PutStr (show result ++ "\n")
    stackCalc
  binTry _ = do
    PutStr ("Fewer than 2 items on the stack\n")
    stackCalc

  try : (minHIn : Nat) -> (minHOut : Nat) -> ({hIn : Nat} -> StackCmd () (minHIn + hIn) (minHOut + hIn)) -> StackIO height
  try minHIn minHOut f {height} = ?fooooo
    where
      tryInside : (n: Nat) -> (m: Nat) -> Maybe (StackCmd () (n + hIn) (someHeight)) -> Maybe (StackCmd () ((S n) + hIn) (someHeight))
      tryInside n m Nothing = Nothing
      tryInside Z other = Nothing
      tryInside (S Z) (S x) = ?foo
      tryInside (S n) Z = Nothing
      tryInside (S n) (S x) = ?foo2


  tryNegate : StackIO height
  tryNegate {height = S h} = do
    doNegate
    res <- Top
    PutStr (show res ++ "\n")
    stackCalc
  tryNegate = do
    PutStr ("Fewer than 1 item on the stack\n")
    stackCalc

  tryDiscard : StackIO height
  tryDiscard {height = S h} = do
    val <- Pop
    PutStr ("Discarded " ++ (show val) ++ "\n")
    stackCalc
  tryDiscard = do
    PutStr "Fewer than 1 item on the stack\n"
    stackCalc

  tryDuplicate : StackIO height
  tryDuplicate {height = S h} = do
    Top >>= Push
    val <- Top
    PutStr ("Duplicated " ++ (show val) ++ "\n")
    stackCalc
  tryDuplicate = do
    PutStr "Fewer than 1 item on the stack\n"
    stackCalc


  stackCalc : StackIO height
  stackCalc = do PutStr "> "
                 input <- GetStr
                 case strToInput input of
                      Nothing => do PutStr "Invalid input\n"
                                    stackCalc
                      Just (Number x) => do Push x
                                            stackCalc
                      Just Add => binTry doAdd
                      Just Subtract => binTry doSubtract
                      Just Multiply => binTry doMultiply
                      Just Negate => tryNegate
                      Just Discard => tryDiscard
                      Just Duplicate => tryDuplicate

main : IO ()
main = run forever [] stackCalc
