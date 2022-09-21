namespace Q1

  data DoorState = DoorOpen | DoorClosed

  data DoorCmd : Type -> DoorState -> DoorState -> Type where
       Open : DoorCmd () DoorClosed DoorOpen
       Close : DoorCmd () DoorOpen DoorClosed
       RingBell : DoorCmd () s s

       Pure : ty -> DoorCmd ty state state
       (>>=) : DoorCmd a state1 state2 ->
               (a -> DoorCmd b state2 state3) ->
               DoorCmd b state1 state3

{- This should typecheck after you modified RingBell. -}

  doorProg : DoorCmd () DoorClosed DoorClosed
  doorProg = do RingBell
                Open
                RingBell
                Close

namespace Q2

  data GuessCmd : Type -> Nat -> Nat -> Type where
       Try : Integer -> GuessCmd Ordering (S x) x

       Pure : ty -> GuessCmd ty state state
       (>>=) : GuessCmd a state1 state2 ->
               (a -> GuessCmd b state2 state3) ->
               GuessCmd b state1 state3


{- This should typecheck. -}

  threeGuesses : GuessCmd () 3 0
  threeGuesses = do Try 10
                    Try 20
                    Try 15
                    Pure ()

{- This should *not* typecheck. -}

{- Bonus question: Can you give no_guesses a type so 
   that it does typecheck? -}

  -- no_guesses : GuessCmd () 0 0
  -- no_guesses = do Try 10
  --                 Pure ()




namespace Q3

  data Matter = Solid | Liquid | Gas

  data MatterCmd : Type -> Matter -> Matter -> Type where
    Melt : MatterCmd () Solid Liquid
    Boil : MatterCmd () Liquid Gas
    Condense : MatterCmd () Gas Liquid
    Freeze : MatterCmd () Liquid Solid

    Pure : ty -> MatterCmd ty s s
    (>>=) : MatterCmd a s1 s2 ->
      (a -> MatterCmd b s2 s3) ->
      MatterCmd b s1 s3

{- This should typeckeck. -}
  iceSteam : MatterCmd () Solid Gas
  iceSteam = do Melt
                Boil

{- This should typeckeck, too. -}
  steamIce : MatterCmd () Gas Solid
  steamIce = do Condense
                Freeze

{- This should *not* typeckeck. -}
  -- overMelt : MatterCmd () Solid Gas
  -- overMelt = do Melt
  --               Melt

