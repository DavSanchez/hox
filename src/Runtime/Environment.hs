{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Runtime.Environment
  ( Environment,
    Frame,
    Globals,
    newGlobals,
    declareGlobal,
    lookupGlobal,
    assignGlobal,
    newFrame,
    readSlot,
    writeSlot,
    popFrame,
    getAtDistance,
    assignAtDistance,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import GHC.Exts
  ( Int (I#),
    RealWorld,
    SmallMutableArray#,
    getSizeofSmallMutableArray#,
    isTrue#,
    newSmallArray#,
    readSmallArray#,
    sameSmallMutableArray#,
    writeSmallArray#,
    (<#),
    (>=#),
  )
import GHC.IO (IO (IO))

-- | Global variables live in a map, by name: they can be declared at any time
-- and referred to before they exist (the latter is a runtime error).
type Globals a = IORef (M.Map Text a)

-- | The frame of a local scope: a fixed number of slots.
--
-- The resolver decides which slot every local variable occupies, so there is
-- no name lookup at runtime. Slots are bounds-checked: a mismatch between the
-- resolver and the interpreter is a bug that must fail loudly instead of
-- reading arbitrary memory.
data Frame a = Frame (SmallMutableArray# RealWorld a)

instance Eq (Frame a) where
  Frame a == Frame b = isTrue# (sameSmallMutableArray# a b)

-- | Innermost scope first.
type Environment a = [Frame a]

newGlobals :: (MonadIO m) => m (Globals a)
newGlobals = liftIO $ newIORef mempty

declareGlobal :: (MonadIO m) => Text -> a -> Globals a -> m ()
declareGlobal name val globals = liftIO $ modifyIORef' globals (M.insert name val)

lookupGlobal :: (MonadIO m) => Text -> Globals a -> m (Maybe a)
lookupGlobal name globals = do
  m <- liftIO $ readIORef globals
  pure $ M.lookup name m

-- | Assigns to an existing global. 'False' if there is no such global.
assignGlobal :: (MonadIO m) => Text -> a -> Globals a -> m Bool
assignGlobal name val globals = do
  m <- liftIO $ readIORef globals
  if M.member name m
    then liftIO (modifyIORef' globals (M.insert name val)) >> pure True
    else pure False

-- | A frame of the given size, with every slot set to the given filler.
newFrame :: (MonadIO m) => Int -> a -> m (Frame a)
newFrame (I# n) filler = liftIO $ IO $ \s ->
  case newSmallArray# n filler s of
    (# s1, arr #) -> (# s1, Frame arr #)
{-# INLINE newFrame #-}

readSlot :: (MonadIO m) => Frame a -> Int -> m a
readSlot (Frame arr) i@(I# i#) = liftIO $ IO $ \s ->
  case getSizeofSmallMutableArray# arr s of
    (# s1, n# #)
      | isTrue# (i# >=# 0#) && isTrue# (i# <# n#) -> readSmallArray# arr i# s1
      | otherwise -> case slotOutOfRange i of IO f -> f s1
{-# INLINE readSlot #-}

writeSlot :: (MonadIO m) => Frame a -> Int -> a -> m ()
writeSlot (Frame arr) i@(I# i#) val = liftIO $ IO $ \s ->
  case getSizeofSmallMutableArray# arr s of
    (# s1, n# #)
      | isTrue# (i# >=# 0#) && isTrue# (i# <# n#) ->
          case writeSmallArray# arr i# val s1 of
            s2 -> (# s2, () #)
      | otherwise -> case slotOutOfRange i of IO f -> f s1
{-# INLINE writeSlot #-}

slotOutOfRange :: Int -> IO a
slotOutOfRange i = ioError (userError ("internal error: environment slot " ++ show i ++ " out of range"))

popFrame :: Environment a -> Environment a
popFrame [] = []
popFrame (_ : xs) = xs

-- | The frame @depth@ scopes up from the innermost one.
frameAt :: (MonadIO m) => Int -> Environment a -> m (Frame a)
frameAt _ [] = liftIO $ ioError (userError "internal error: scope distance out of range")
frameAt 0 (f : _) = pure f
frameAt n (_ : fs) = frameAt (n - 1) fs

-- Interaction with the (depth, slot) pairs computed by the resolver.
getAtDistance :: (MonadIO m) => Int -> Int -> Environment a -> m a
getAtDistance depth slot env = do
  frame <- frameAt depth env
  readSlot frame slot
{-# INLINE getAtDistance #-}

assignAtDistance :: (MonadIO m) => Int -> Int -> a -> Environment a -> m ()
assignAtDistance depth slot val env = do
  frame <- frameAt depth env
  writeSlot frame slot val
{-# INLINE assignAtDistance #-}
