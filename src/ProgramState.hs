module ProgramState
  ( ProgramState (..),
    pushEnvFrame,
    popEnvFrame,
    declareEnvVarRef,
    findEnvVarRef,
    getEnvVarRef,
    updateEnv,
  )
where

import Data.IORef (IORef)
import Data.Map qualified as M
import Environment (Environment, declareVarRef, findVarRef, popFrame, pushFrame)

data ProgramState a = ProgramState
  { environment :: Environment a,
    globals :: Environment a,
    locals :: M.Map String Int
  }

-- Transformation functions

pushEnvFrame :: ProgramState a -> ProgramState a
pushEnvFrame ps = ps {environment = pushFrame (environment ps)}

popEnvFrame :: ProgramState a -> ProgramState a
popEnvFrame ps = ps {environment = popFrame (environment ps)}

declareEnvVarRef :: String -> IORef a -> ProgramState a -> ProgramState a
declareEnvVarRef name ref ps = ps {environment = declareVarRef name ref (environment ps)}

findEnvVarRef :: String -> ProgramState a -> Maybe (IORef a)
findEnvVarRef name ps =
  let env = environment ps
   in findVarRef name env

getEnvVarRef :: String -> ProgramState a -> Maybe (IORef a)
getEnvVarRef name ps = let env = environment ps in findVarRef name env

updateEnv :: Environment a -> ProgramState a -> ProgramState a
updateEnv env ps = ps {environment = env}
