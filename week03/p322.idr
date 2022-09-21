import Data.Primitives.Views
import System

%default total

data Command : Type -> Type where
     PutStr : String -> Command ()
     GetLine : Command String
     ReadFile : String -> Command (Either FileError String)
     WriteFile : (fileName : String) -> (contents : String) -> Command (Either FileError ())

     Pure : ty -> Command ty
     Bind : Command a -> (a -> Command b) -> Command b

data ConsoleIO : Type -> Type where
     Quit : a -> ConsoleIO a
     Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

namespace CommandDo
  (>>=) : Command a -> (a -> Command b) -> Command b
  (>>=) = Bind

namespace ConsoleDo
  (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do

data Fuel = Dry | More (Lazy Fuel)

runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine
runCommand (Pure val) = pure val
runCommand (Bind c f) = do res <- runCommand c
                           runCommand (f res)
runCommand (ReadFile fileName) = readFile fileName
runCommand (WriteFile fileName contents) = writeFile fileName contents

run : Fuel -> ConsoleIO a -> IO (Maybe a)
run fuel (Quit val) = do pure (Just val)
run (More fuel) (Do c f) = do res <- runCommand c
                              run fuel (f res)
run Dry p = pure Nothing

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'

arithInputs : Int -> Stream Int
arithInputs seed = map bound (randoms seed)
  where
    bound : Int -> Int
    bound x with (divides x 12)
      bound ((12 * div) + rem) | (DivBy prf) = abs rem + 1

mutual
  correct : Stream Int -> (score : Nat) -> (tot : Nat) -> ConsoleIO (Nat, Nat)
  correct nums score tot
          = do PutStr "Correct!\n"
               quiz nums (score + 1) (tot + 1)

  wrong : Stream Int -> Int -> (score : Nat) -> (tot : Nat) -> ConsoleIO (Nat, Nat)
  wrong nums ans score tot
        = do PutStr ("Wrong, the answer is " ++ show ans ++ "\n")
             quiz nums score (tot + 1)

  data Input = Answer Int
             | QuitCmd

  readInput : (prompt : String) -> Command Input
  readInput prompt
     = do PutStr prompt
          answer <- GetLine
          if toLower answer == "quit"
             then Pure QuitCmd
             else Pure (Answer (cast answer))

  quiz : Stream Int -> (score : Nat) -> (tot : Nat) -> ConsoleIO (Nat, Nat)
  quiz (num1 :: num2 :: nums) score tot
     = do PutStr ("Score so far: " ++ show score ++ " / " ++ show tot ++ "\n")
          input <- readInput (show num1 ++ " * " ++ show num2 ++ "? ")
          case input of
               Answer answer => if answer == num1 * num2
                                   then correct nums score tot
                                   else wrong nums (num1 * num2) score tot
               QuitCmd => do res <- WriteFile "/home/monner/study/ASA/week03/score/scores" (show score ++ " / " ++ show tot)
                             case res of
                               Left => PutStr "Couldn't write score file\n"
                               Right => Pure ()
                             Quit (score, tot) -- Error above is totes fine

partial
forever : Fuel
forever = More forever

partial
main : IO ()
main = do seed <- time
          Just score <- run forever (quiz (arithInputs (fromInteger seed)) 0 0)
               | Nothing => putStrLn "Ran out of fuel"
          putStrLn ("Final score: " ++ show (fst score) ++ " / " ++ show (snd score))



data ShellInput = Cat String
            | Copy String String
            | Exit

readShellInput : Command (Maybe ShellInput)
readShellInput = do
  PutStr "> "
  input <- GetLine
  case words (toLower input) of
    "cat" :: fn :: [] => Pure (Just (Cat fn))
    "copy" :: src :: dst :: [] => Pure (Just (Copy src dst))
    "exit" :: _ => Pure (Just (Exit))
    "e" :: _ => Pure (Just (Exit))
    "quit" :: _ => Pure (Just (Exit))
    "q" :: _ => Pure (Just (Exit))
    _ => Pure Nothing

printError : (err : FileError) -> (fn : String) -> Command ()
printError (GenericFileError x) fn = PutStr ("Error '" ++ show x ++ "' with file '" ++ fn ++ "'\n")
printError FileReadError fn = PutStr ("Could not read file '" ++ fn ++ "'\n")
printError FileWriteError fn = PutStr ("Could not write to file '" ++ fn ++ "'\n")
printError FileNotFound fn = PutStr ("File '" ++ fn ++ "' not found\n")
printError PermissionDenied fn = PutStr ("Permission denied: '" ++ fn ++ "'\n")

mutual
  copy : (src: String) -> (dest: String) -> ConsoleIO ()
  copy src dest = do
    Right contents <- ReadFile src | Left err => do
      printError err src
      shell
    Right () <- WriteFile dest contents | Left err => do
      printError err dest
      shell
    shell

  cat : (fn : String) -> ConsoleIO ()
  cat fn = do
    Right contents <- ReadFile fn | Left err => do
      printError err fn
      shell
    PutStr contents
    PutStr "\n"
    shell

  shell : ConsoleIO ()
  shell = do
    Just cmd <- readShellInput | Nothing => do
      PutStr "Unrecognized command\n"
      shell
    case cmd of
      Exit => Quit ()
      (Cat fn) => cat fn
      (Copy src dst) => copy src dst

partial
shellCLI : IO ()
shellCLI = do
  putStrLn "Welcome to the smallest, shittiest CLI."
  putStrLn "Commands are 'cat [filename]', 'copy [src] [dest]' and 'exit'."
  Just () <- run forever shell | Nothing => putStrLn "Ran out of fuel"
  putStrLn "Exited."
