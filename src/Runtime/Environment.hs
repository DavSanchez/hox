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
    frameSize,
    readSlot,
    writeSlot,
    popFrame,
    getAtDistance,
    assignAtDistance,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
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

-- | The table of global variables.
--
-- The resolver gives every global name an index, so a global is read and
-- written by index like a local. A global is 'Nothing' until it is declared.
-- Globals can be declared at any point of the program, and the table is
-- created before the program is resolved, so it grows on demand.
type Globals a = IORef (Frame (Maybe a))

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
newGlobals = do
  table <- newFrame initialGlobals Nothing
  liftIO $ newIORef table
  where
    initialGlobals = 16

declareGlobal :: (MonadIO m) => Int -> a -> Globals a -> m ()
declareGlobal slot val globals = do
  table <- liftIO $ readIORef globals
  size <- frameSize table
  if slot < size
    then writeSlot table slot (Just $! val)
    else do
      -- Grow, at least geometrically so that declaring n globals stays linear.
      bigger <- newFrame (max (slot + 1) (2 * size)) Nothing
      for_ [0 .. size - 1] $ \i -> readSlot table i >>= writeSlot bigger i
      writeSlot bigger slot (Just $! val)
      liftIO $ writeIORef globals bigger

-- | 'Nothing' if the global has not been declared (including an index the
-- resolver never handed out).
lookupGlobal :: (MonadIO m) => Int -> Globals a -> m (Maybe a)
lookupGlobal slot globals = do
  table <- liftIO $ readIORef globals
  size <- frameSize table
  if slot >= 0 && slot < size then readSlot table slot else pure Nothing
{-# INLINE lookupGlobal #-}

-- | Assigns to a declared global. 'False' if there is no such global.
assignGlobal :: (MonadIO m) => Int -> a -> Globals a -> m Bool
assignGlobal slot val globals = do
  table <- liftIO $ readIORef globals
  size <- frameSize table
  if slot >= 0 && slot < size
    then do
      current <- readSlot table slot
      case current of
        Nothing -> pure False
        Just _ -> writeSlot table slot (Just $! val) >> pure True
    else pure False
{-# INLINE assignGlobal #-}

-- | A frame of the given size, with every slot set to the given filler.
newFrame :: (MonadIO m) => Int -> a -> m (Frame a)
newFrame (I# n) filler = liftIO $ IO $ \s ->
  case newSmallArray# n filler s of
    (# s1, arr #) -> (# s1, Frame arr #)
{-# INLINE newFrame #-}

frameSize :: (MonadIO m) => Frame a -> m Int
frameSize (Frame arr) = liftIO $ IO $ \s ->
  case getSizeofSmallMutableArray# arr s of
    (# s1, n# #) -> (# s1, I# n# #)
{-# INLINE frameSize #-}

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
