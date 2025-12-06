module Environment
  ( Environment,
    declareVar,
    pushFrame,
    popFrame,
    newEnv,
    findVar,
    assignVar,
    newFromEnv,
  )
where

import Data.Functor (($>))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Data.Map qualified as M

type Environment a = NonEmpty (Frame a)

type Frame a = IORef (M.Map String a)

newEnv :: IO (Environment a)
newEnv = do
  frame <- newIORef mempty
  pure $ frame :| []

newFromEnv :: Environment a -> IO (Environment a)
newFromEnv = pushFrame

pushFrame :: Environment a -> IO (Environment a)
pushFrame env = do
  frame <- newIORef mempty
  pure (frame <| env)

popFrame :: Environment a -> Environment a
popFrame single@(_ :| []) = single -- cannot pop the last frame
popFrame (_ :| (x : xs)) = x :| xs

-- | Inserts the defined variable ref in the current environment frame (i.e. top of the stack)
-- This is only for declarations
declareVar :: String -> a -> Environment a -> IO ()
declareVar name ref (frame :| _rest) = modifyIORef' frame (M.insert name ref)

-- | Assigns a value to an existing variable in the environment.
-- Traverses down the stack until it finds a frame that contains the variable,
-- and updates its value. Returns True if the variable was found and updated,
-- False otherwise.
assignVar :: String -> a -> Environment a -> IO Bool
assignVar name value (frame :| rest) = do
  currentMap <- readIORef frame
  if M.member name currentMap
    then modifyIORef' frame (M.insert name value) $> True
    else case rest of
      [] -> pure False
      (nextFrame : xs) -> assignVar name value (nextFrame :| xs)

-- | Finds the variable reference in the environment.
-- Traverses down the stack until it finds a frame that contains the variable,
-- and returns its value. If not found, returns Nothing.
findVar :: String -> Environment a -> IO (Maybe a)
findVar name (frame :| rest) = do
  currentMap <- readIORef frame
  case M.lookup name currentMap of
    Just val -> pure (Just val)
    Nothing -> case rest of
      [] -> pure Nothing
      (nextFrame : xs) -> findVar name (nextFrame :| xs)

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
