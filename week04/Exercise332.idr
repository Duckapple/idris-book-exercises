import Control.Monad.State

data Tree a = Empty
            | Node (Tree a) a (Tree a)

{- 1 -}

update : (stateType -> stateType) -> State stateType ()
update f = do
  st <- get
  put $ f st

{- 2 -}

countEmpty : Tree a -> State Nat ()
countEmpty Empty = do
  st <- get
  put $ S st
countEmpty (Node l x r) = do
  countEmpty l
  countEmpty r

testTree : Tree Nat
testTree = Node (Node Empty 4 (Node Empty 2 Empty)) 1 Empty

{- 3 -}

countEmptyNode : Tree a -> State (Nat, Nat) ()
countEmptyNode Empty = do
  (es, ns) <- get
  put $ (S es, ns)
countEmptyNode (Node l x r) = do
  countEmptyNode l
  (es, ns) <- get
  put $ (es, S ns)
  countEmptyNode r
