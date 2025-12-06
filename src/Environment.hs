module Environment
  ( Environment,
    declareVarRef,
    pushFrame,
    popFrame,
    newEnv,
    findVarRef,
    newFromEnv,
  )
where

import Control.Monad (join)
import Data.Foldable (find)
import Data.IORef (IORef)
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Data.Map qualified as M
import Data.Maybe (isJust)

type Environment a = NonEmpty (Frame a)

type Frame a = M.Map String (IORef a)

newEnv :: Environment a
newEnv = mempty :| []

newFromEnv :: Environment a -> Environment a
newFromEnv = pushFrame

pushFrame :: Environment a -> Environment a
pushFrame = (mempty <|) -- prepend a new empty frame

popFrame :: Environment a -> Environment a
popFrame single@(_ :| []) = single -- cannot pop the last frame
popFrame (_ :| (x : xs)) = x :| xs

-- Inserts the defined variable ref in the current environment frame (i.e. top of the stack)
-- This is only for declarations
declareVarRef :: String -> IORef a -> Environment a -> Environment a
declareVarRef name ref (frame :| rest) = M.insert name ref frame :| rest

-- | Finds the variable reference in the environment.
-- Traverses down the stack until it finds a frame that contains the variable,
-- and returns its IORef. If not found, returns Nothing.
findVarRef :: String -> Environment a -> Maybe (IORef a)
findVarRef name env = join $ find isJust (M.lookup name <$> env)

{-
-- Pretty printing

-- | Render the environment stack as ASCII boxes, top (current frame) first.
-- Each box lists variables and their values, with arrows pointing to parent frames.
renderEnvBoxes :: (Show a) => Environment a -> IO String
renderEnvBoxes env = do
  frames <- for (zip [0 ..] (toList env)) $ \(i, frame) -> do
    entries <- for (M.toList frame) $ \(name, ref) -> do
      val <- readIORef ref
      pure (name, val)
    pure (i, entries)
  pure $ intercalate "\n" (concatMap boxWithArrow (annotateParents frames))
  where
    annotateParents :: [(Int, [(String, a)])] -> [((Int, [(String, a)]), Bool)]
    annotateParents xs =
      let n = length xs
       in [(x, idx < n - 1) | (idx, x) <- zip [0 ..] xs]

    boxWithArrow :: (Show a) => ((Int, [(String, a)]), Bool) -> [String]
    boxWithArrow ((i, entries), hasParent) =
      let title = "Frame " ++ show i ++ if i == 0 then " (current)" else ""
          contentLines = case entries of
            [] -> ["<empty>"]
            xs -> [name ++ " = " ++ show val | (name, val) <- xs]
          width = maximum (length title : map length contentLines) + 2 -- padding
          top = "+" ++ replicate width '-' ++ "+"
          midTitle = "| " ++ padRight width title ++ "|"
          mids = ["| " ++ padRight width ln ++ "|" | ln <- contentLines]
          bottom = "+" ++ replicate width '-' ++ "+"
          box = [top, midTitle] ++ mids ++ [bottom]
       in if hasParent
            then box ++ [centerArrow width]
            else box

    padRight :: Int -> String -> String
    padRight w s = s ++ replicate (w - length s) ' '

    centerArrow :: Int -> String
    centerArrow w =
      let mid = w `div` 2
          left = replicate mid ' '
       in left ++ "|" ++ "\n" ++ left ++ "v"

-}
