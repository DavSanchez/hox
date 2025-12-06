module ProgramState
  ( ProgramState (..),
  )
where

import Data.Map qualified as M
import Environment (Environment)

data ProgramState a = ProgramState
  { environment :: Environment a,
    globals :: Environment a,
    locals :: M.Map String Int
  }
