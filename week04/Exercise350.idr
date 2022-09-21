import Data.Primitives.Views
import System

%default total

{- auxiliary definitions for 1 & 2 -}

record Score where
       constructor MkScore
       correct : Nat
       attempted : Nat

record GameState where
       constructor MkGameState
       score : Score
       difficulty : Int

Show GameState where
    show st = show (correct (score st)) ++ "/"
              ++ show (attempted (score st)) ++ "\n"
              ++ "Difficulty: " ++ show (difficulty st)

initState : GameState
initState = MkGameState (MkScore 0 0) 12

addWrong : GameState -> GameState
addWrong = record { score->attempted $= (+1) }

addCorrect : GameState -> GameState
addCorrect = record { score->correct $= (+1),
                      score->attempted $= (+1) }

setDifficulty : Int -> GameState -> GameState
setDifficulty newDiff state = record { difficulty = newDiff } state

data Command : Type -> Type where
     PutStr : String -> Command ()
     GetLine : Command String

     GetRandom : Command Int
     GetGameState : Command GameState
     PutGameState : GameState -> Command ()

     Pure : ty -> Command ty
     Bind : Command a -> (a -> Command b) -> Command b

data ConsoleIO : Type -> Type where
     Quit : a -> ConsoleIO a
     Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b


(>>=) : Command a -> (a -> Command b) -> Command b
(>>=) = Bind

namespace ConsoleDo
  (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do


randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'


runCommand : Stream Int -> GameState -> Command a -> 
             IO (a, Stream Int, GameState)
runCommand rnds state (PutStr x) = do putStr x
                                      pure ((), rnds, state)
runCommand rnds state GetLine = do str <- getLine
                                   pure (str, rnds, state)

runCommand (val :: rnds) state GetRandom
      = pure (getRandom val (difficulty state), rnds, state)
  where
    getRandom : Int -> Int -> Int
    getRandom val max with (divides val max)
      getRandom val 0 | DivByZero = 1
      getRandom ((max * div) + rem) max | (DivBy prf) = abs rem + 1
runCommand rnds state GetGameState 
      = pure (state, rnds, state)
runCommand rnds state (PutGameState newState) 
      = pure ((), rnds, newState)

runCommand rnds state (Pure val)
      = pure (val, rnds, state)
runCommand rnds state (Bind c f)
      = do (res, newRnds, newState) <- runCommand rnds state c
           runCommand newRnds newState (f res)


data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

run : Fuel -> Stream Int -> GameState -> ConsoleIO a -> 
      IO (Maybe a, Stream Int, GameState)
run fuel rnds state (Quit val) = do pure (Just val, rnds, state)
run (More fuel) rnds state (Do c f) 
     = do (res, newRnds, newState) <- runCommand rnds state c
          run fuel newRnds newState (f res)
run Dry rnds state p = pure (Nothing, rnds, state)


{- 1 -}

updateGameState : (GameState -> GameState) -> Command ()
updateGameState f = do
  st <- GetGameState
  PutGameState (f st)

{- testing updateGameState -}

mutual
  correct : ConsoleIO GameState
  correct = do PutStr "Correct!\n"
               updateGameState addCorrect
               quiz 

  wrong : Int -> ConsoleIO GameState
  wrong ans 
        = do PutStr ("Wrong, the answer is " ++ show ans ++ "\n")
             updateGameState addWrong
             quiz 
  
  data Input = Answer Int
             | QuitCmd

  readInput : (prompt : String) -> Command Input
  readInput prompt 
     = do PutStr prompt
          answer <- GetLine
          if toLower answer == "quit" 
             then Pure QuitCmd 
             else Pure (Answer (cast answer))

  quiz : ConsoleIO GameState
  quiz = do num1 <- GetRandom
            num2 <- GetRandom
            st <- GetGameState
            PutStr (show st ++ "\n")

            input <- readInput (show num1 ++ " * " ++ show num2 ++ "? ")
            case input of
               Answer answer => if answer == num1 * num2 
                                   then correct
                                   else wrong (num1 * num2) 
               QuitCmd => Quit st

partial
main : IO ()
main = do seed <- time
          (Just score, _, state) <- 
              run forever (randoms (fromInteger seed)) initState quiz
                  | _ => putStrLn "Ran out of fuel"
          putStrLn ("Final score: " ++ show state)


{- 2 -}

mutual
  Functor Command where
    map f a = Bind a (\x => Pure (f x))
    
  Applicative Command where
    pure = Pure
    (<*>) f a = do
      a' <- a
      f' <- f
      pure (f' a')

  Monad Command where
    (>>=) = Bind
    join x = do
      cmds <- x
      cmds



{- auxiliary definitions for 3 & 4 -}



record Votes where
       constructor MkVotes
       upvotes : Integer
       downvotes : Integer

record Article where
       constructor MkArticle
       title : String
       url : String
       score : Votes

initPage : (title : String) -> (url : String) -> Article
initPage title url = MkArticle title url (MkVotes 0 0)

badSite : Article
badSite = MkArticle "Bad Page" "http://example.com/bad" (MkVotes 5 47)

goodSite : Article
goodSite = MkArticle "Good Page" "http://example.com/good" (MkVotes 101 7)

{- 3 -}

getScore : Article -> Integer
getScore (MkArticle title url (MkVotes upvotes downvotes)) = upvotes - downvotes


{- 4 -}

addUpvote : Article -> Article
addUpvote = record { score->upvotes $= (+1) }

addDownvote : Article -> Article
addDownvote = record { score->downvotes $= (+1) }
