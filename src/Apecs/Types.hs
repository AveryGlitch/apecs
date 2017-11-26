{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Apecs.Types where

import Control.Monad.Reader
import Data.Traversable (for)
import qualified Data.Vector.Unboxed as U
import qualified Apecs.THTuples as T

-- | An Entity is really just an Int. The type variable is used to keep track of reads and writes, but can be freely cast.
newtype Entity c = Entity {unEntity :: Int} deriving (Eq, Ord, Show)

-- | A slice is a list of entities, represented by a Data.Unbox.Vector of Ints.
newtype Slice c = Slice {unSlice :: U.Vector Int} deriving (Show, Monoid)

-- | A system is a newtype around `ReaderT w IO a`, where `w` is the game world variable.
newtype SystemT w m a = System {unSystem :: ReaderT w m a} deriving (Functor, Monad, Applicative, MonadTrans, MonadIO)
type System w a = SystemT w IO a

-- | A component is defined by the type of its storage
--   The storage in turn supplies runtime types for the component.
--   For the component to be valid, its Storage must be in instance of Store.
class (Stores (Storage c) ~ c) => Component c where
  type Storage c

-- | A world `Has` a component if it can produce its Storage
class (Store m (Storage c), Component c) => Has w m c where
  getStore :: Monad m => SystemT w m (Storage c)

-- | Represents a safe access to @c@. A safe access is either a read that might fail, or a write that might delete.
newtype Safe c = Safe {getSafe :: SafeRW (Storage c)}

-- | Holds components indexed by entities
class Monad m => Store m s where
  -- | The type of components stored by this Store
  type Stores s
  -- | Return type for safe reads writes to the store
  type SafeRW s

  -- Initialize the store with its initialization arguments.
  initStore :: m s

  -- | Retrieves a component from the store
  explGet :: s -> Int -> m (SafeRW s)
  -- | Writes a component
  explSet :: s -> Int -> Stores s -> m ()
  -- | Destroys the component for the given index.
  explDestroy :: s -> Int -> m ()
  -- | Returns whether there is a component for the given index
  {-# INLINE explExists #-}
  explExists :: s -> Int -> m Bool
  explExists s n = do
    mems <- explMembers s
    return $ U.elem n mems

  -- | Returns an unboxed vector of member indices
  explMembers :: s -> m (U.Vector Int)

  -- | Unsafe index to the store. What happens if the component does not exist is left undefined.
  explGetUnsafe :: s -> Int -> m (Stores s)
  -- | Either writes or deletes a component
  explSetMaybe :: s -> Int -> SafeRW s -> m ()

  -- | Removes all components.
  --   Equivalent to calling @explDestroy@ on each member
  {-# INLINE explReset #-}
  explReset :: s -> m ()
  explReset s = do
    sl <- explMembers s
    U.mapM_ (explDestroy s) sl

  -- | Monadically iterates over member indices
  explImapM_ :: s -> (Int -> m a) -> m ()
  {-# INLINE explImapM_ #-}
  explImapM_ s ma = explMembers s >>= mapM_ ma . U.toList

  -- | Monadically iterates over member indices
  explImapM :: s -> (Int -> m a) -> m [a]
  {-# INLINE explImapM #-}
  explImapM s ma = explMembers s >>= mapM ma . U.toList

  -- | Modifies an element in the store.
  --   Equivalent to reading a value, and then writing the result of the function application.
  {-# INLINE explModify #-}
  explModify :: s -> Int -> (Stores s -> Stores s) -> m ()
  explModify s ety f = do etyExists <- explExists s ety
                          when etyExists $ explGetUnsafe s ety >>= explSet s ety . f

  -- | Maps over all elements of this store.
  --   Equivalent to getting a list of all entities with this component, and then explModifying each of them.
  explCmap :: s -> (Stores s -> Stores s) -> m ()
  {-# INLINE explCmap #-}
  explCmap s f = explMembers s >>= U.mapM_ (\ety -> explModify s ety f)

  explCmapM_ :: s -> (Stores s -> m a) -> m ()
  {-# INLINE explCmapM_ #-}
  explCmapM_ s sys = do
    sl <- explMembers s
    U.forM_ sl $ \ety -> do x :: Stores s <- explGetUnsafe s ety
                            sys x

  explCimapM_ :: s -> ((Int, Stores s) -> m a) -> m ()
  {-# INLINE explCimapM_ #-}
  explCimapM_ s sys = do
    sl <- explMembers s
    U.forM_ sl $ \ety -> do x :: Stores s <- explGetUnsafe s ety
                            sys (ety,x)

  explCmapM  :: s -> (Stores s -> m a) -> m [a]
  {-# INLINE explCmapM #-}
  explCmapM s sys = do
    sl <- explMembers s
    for (U.toList sl) $ \ety -> do
      x :: Stores s <- explGetUnsafe s ety
      sys x

  explCimapM :: s -> ((Int, Stores s) -> m a) -> m [a]
  {-# INLINE explCimapM #-}
  explCimapM s sys = do
    sl <- explMembers s
    for (U.toList sl) $ \ety -> do
      x :: Stores s <- explGetUnsafe s ety
      sys (ety,x)

-- | Class of storages for global values, @get undefined@ is expected to be safe.
class (SafeRW s ~ Stores s) => GlobalStore s where

-- | Casts for entities and slices
class Cast m where cast :: forall a. m a -> forall b. m b

instance Cast Entity where
  {-# INLINE cast #-}
  cast (Entity ety) = Entity ety
instance Cast Slice where
  {-# INLINE cast #-}
  cast (Slice vec) = Slice vec

-- Tuple Instances
T.makeInstances [2..6]
instance (GlobalStore a, GlobalStore b) => GlobalStore (a,b) where
{-instance (GlobalStore a, GlobalStore b, GlobalStore c) => GlobalStore (a,b,c) where-}
