data Access = LoggedOut | LoggedIn
data PwdCheck = Correct | Incorrect | Correctish

chkToAccess : PwdCheck -> Access
chkToAccess Correct = LoggedIn
chkToAccess _ = LoggedOut

data ShellCmd : (ty : Type) -> Access -> (ty -> Access) -> Type where
  Password : String -> ShellCmd PwdCheck LoggedOut (Main.chkToAccess)
  Logout : ShellCmd () LoggedIn (const LoggedOut)
  GetSecret : ShellCmd String LoggedIn (const LoggedIn)

  PutStr : String -> ShellCmd () state (const state)
  Pure : (res : ty) -> ShellCmd ty (state_fn res) state_fn 
  
  (>>=) : ShellCmd a state1 state2_fn -> 
          ((res : a) -> ShellCmd b (state2_fn res) state3_fn) -> 
          ShellCmd b state1 state3_fn


-- The following function should type-check if you have the correct answer:

session : ShellCmd () LoggedOut (const LoggedOut)
session = do Correct <- Password "wurzel"
                | Incorrect => PutStr "Wrong password"
                | Correctish => PutStr "password not quite correct"
             msg <- GetSecret
             PutStr ("Secret code: " ++ show msg ++ "\n")
             Logout


-- The following functions should not type-check:

-- badSession : ShellCmd () LoggedOut (const LoggedOut)
-- badSession = do Password "wurzel"
--                 msg <- GetSecret
--                 PutStr ("Secret code: " ++ show msg ++ "\n")
--                 Logout
-- noLogout : ShellCmd () LoggedOut (const LoggedOut)
-- noLogout = do Correct <- Password "wurzel"
--                  | Incorrect => PutStr "Wrong password"
--               msg <- GetSecret
--               PutStr ("Secret code: " ++ show msg ++ "\n")
