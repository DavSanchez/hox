module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Unique -- This provides "Object Identity" capabilities

-- ==========================================
-- Approach 1: Pure Values (The Default)
-- ==========================================
-- Values are defined by their content.
newtype Expr = Var String deriving stock (Show, Eq, Ord)

demoPure :: IO ()
demoPure = do
  putStrLn "--- Approach 1: Pure Values (Default) ---"
  let val1 = Var "a"
  let val2 = Var "a"

  putStrLn $ "val1: " ++ show val1
  putStrLn $ "val2: " ++ show val2
  putStrLn $ "Are they equal? " ++ show (val1 == val2)

  let m = Map.fromList [(val1, 1)]
  -- This overwrites because val1 and val2 are structurally identical
  let m' = Map.insert val2 2 m

  putStrLn $ "Value for val1: " ++ show (Map.lookup val1 m')
  putStrLn $ "Value for val2: " ++ show (Map.lookup val2 m')
  putStrLn ""

-- ==========================================
-- Approach 2: Simulating References with Unique IDs
-- ==========================================
-- This is how we get "Reference Semantics" in Haskell.
-- We explicitly attach a unique identifier generated in IO.

data RefExpr = RefExpr Unique String

-- We define Equality and Ordering based ONLY on the Unique ID
instance Eq RefExpr where
  (RefExpr u1 _) == (RefExpr u2 _) = u1 == u2

instance Ord RefExpr where
  compare (RefExpr u1 _) (RefExpr u2 _) = compare u1 u2

instance Show RefExpr where
  show (RefExpr u s) = "RefExpr(" ++ show (hashUnique u) ++ ") " ++ show s

demoReference :: IO ()
demoReference = do
  putStrLn "--- Approach 2: Simulating References (Data.Unique) ---"

  -- We must create these in the IO monad, just like 'new' in Java
  id1 <- newUnique
  id2 <- newUnique

  let ref1 = RefExpr id1 "a"
  let ref2 = RefExpr id2 "a"

  putStrLn $ "ref1: " ++ show ref1
  putStrLn $ "ref2: " ++ show ref2
  putStrLn $ "Are they equal? " ++ show (ref1 == ref2)

  let m = Map.fromList [(ref1, 1)]
  -- This creates a NEW entry because id1 /= id2
  let m' = Map.insert ref2 42 m

  putStrLn $ "Value for ref1: " ++ show (Map.lookup ref1 m')
  putStrLn $ "Value for ref2: " ++ show (Map.lookup ref2 m')

main :: IO ()
main = do
  putStrLn "=== Haskell Reference Types Demo ===\n"
  demoPure
  demoReference
