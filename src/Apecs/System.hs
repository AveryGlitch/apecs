{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}

module Apecs.System where

import           Control.Monad.Reader
import qualified Data.Vector.Unboxed  as U

import           Apecs.Types

-- | Run a system with a game world
{-# INLINE runSystem #-}
runSystem :: SystemT w m a -> w -> m a
runSystem sys = runReaderT (unSystem sys)

-- | Run a system with a game world
{-# INLINE runWith #-}
runWith :: w -> SystemT w m a -> m a
runWith = flip runSystem

-- | A slice containing all entities with component @c@
{-# INLINE owners #-}
owners :: forall w m c. Has w m c => SystemT w m (Slice c)
owners = do s :: Storage c <- getStore
            lift$ Slice <$> explMembers s

-- | Returns whether the given entity has component @c@
--   For composite components, this indicates whether the component
--   has all its constituents
{-# INLINE exists #-}
exists :: forall w m c. Has w m c => Entity c -> SystemT w m Bool
exists (Entity n) = do s :: Storage c <- getStore
                       lift$ explExists s n

-- | Destroys the component @c@ for the given entity
{-# INLINE destroy #-}
destroy :: forall w m c. Has w m c => Entity c -> SystemT w m ()
destroy (Entity n) = do s :: Storage c <- getStore
                        lift$ explDestroy s n

-- | Removes all components. Equivalent to manually iterating and deleting, but usually optimized.
{-# INLINE resetStore #-}
resetStore :: forall w m c p. Has w m c => p c -> SystemT w m ()
resetStore _ = do s :: Storage c <- getStore
                  lift$ explReset s

-- Setting/Getting
-- | Gets the component for a given entity.
--   This is a safe access, because the entity might not have the requested components.
{-# INLINE get #-}
get :: forall w m c. Has w m c => Entity c -> SystemT w m (Safe c)
get (Entity ety) = do s :: Storage c <- getStore
                      lift$ Safe <$> explGet s ety

-- | Same as @get@, but does not return a safe value and therefore errors if the target component is not present.
{-# INLINE getUnsafe #-}
getUnsafe :: forall w c. Has w c => Entity c -> System w c
getUnsafe (Entity ety) = do s :: Storage c <- getStore
                            liftIO$ explGetUnsafe s ety

-- | Writes a component to a given entity. Will overwrite existing components.
--   The type was originally 'Entity c -> c -> System w ()', but is relaxed to 'Entity e'
--   so you don't always have to write 'set . cast'
{-# INLINE set #-}
set :: forall w m c e. Has w m c => Entity e -> c -> SystemT w m ()
set (Entity ety) x = do
  s :: Storage c <- getStore
  lift$ explSet s ety x

-- | Same as @set@, but uses Safe to possibly delete a component
{-# INLINE set' #-}
set' :: forall w m c. Has w m c => Entity c -> Safe c -> SystemT w m ()
set' (Entity ety) (Safe c) = do
  s :: Storage c <- getStore
  lift$ explSetMaybe s ety c

-- | Applies a function if possible. Equivalent to reading, mapping, and writing, but stores can provide optimized implementations.
{-# INLINE modify #-}
modify :: forall w m c. Has w m c => Entity c -> (c -> c) -> SystemT w m ()
modify (Entity ety) f = do
  s :: Storage c <- getStore
  lift$ explModify s ety f

{-# INLINE imapM_ #-}
-- | Monadically iterate a system over all entities that have that component.
--   Note that writing to the store while iterating over it is undefined behaviour.
imapM_ :: forall w m c. Has w m c => (Entity c -> SystemT w m ()) -> SystemT w m ()
imapM_ sys = do s :: Storage c <- getStore
                w <- System ask
                let f = runWith w . sys . Entity
                lift (explImapM_ s f)


{-# INLINE imapM #-}
-- | Monadically iterate a system over all entities that have that component.
--   Note that writing to the store while iterating over it is undefined behaviour.
imapM :: forall w m c a. Has w m c => (Entity c -> SystemT w m a) -> SystemT w m [a]
imapM sys = do s :: Storage c <- getStore
               w <- System ask
               let f = runWith w . sys . Entity
               lift$ explImapM s f

-- | Maps a pure function over all components
{-# INLINE cmap #-}
cmap :: forall w m c. Has w m c => (c -> c) -> SystemT w m ()
cmap f = do s :: Storage c <- getStore
            lift$ explCmap s f

-- | 'mapM_' version of 'cmap'
{-# INLINE cmapM_ #-}
cmapM_ :: forall w m c. Has w m c => (c -> SystemT w m ()) -> SystemT w m ()
cmapM_ sys = do s :: Storage c <- getStore
                w <- System ask
                let f = runWith w . sys
                lift$ explCmapM_ s f

-- | indexed 'cmapM_', also gives the current entity.
{-# INLINE cimapM_ #-}
cimapM_ :: forall w m c. Has w m c => ((Entity c, c) -> SystemT w m ()) -> SystemT w m ()
cimapM_ sys = do s :: Storage c <- getStore
                 w <- System ask
                 let f (e,c) = runWith w $ sys (Entity e,c)
                 lift$ explCimapM_ s f

-- | mapM version of cmap. Can be used to get a list of entities
--   As the type signature implies, and unlike 'cmap', the return value is not written to the component store.
{-# INLINE cmapM #-}
cmapM :: forall w m c a. Has w m c => (c -> SystemT w m a) -> SystemT w m [a]
cmapM sys = do s :: Storage c <- getStore
               w <- System ask
               let f = runWith w . sys
               lift$ explCmapM s f

-- | indexed 'cmapM', also gives the current entity.
{-# INLINE cimapM #-}
cimapM :: forall w m c a. Has w m c => ((Entity c, c) -> SystemT w m a) -> SystemT w m [a]
cimapM sys = do s :: Storage c <- getStore
                w <- System ask
                let f (e,c) = runWith w $ sys (Entity e,c)
                lift$ explCimapM s f

-- | Maps a function that might delete its components
{-# INLINE cmap' #-}
cmap' :: forall w m c. Has w m c => (c -> Safe c) -> SystemT w m ()
cmap' f = do s :: Storage c <- getStore
             lift$ do sl <- explMembers s
                      U.forM_ sl $ \e -> do
                        r <- explGetUnsafe s e
                        explSetMaybe s e (getSafe . f $ r)

-- | Maps a function over all entities with a @r@, and writes their @w@
{-# INLINE rmap #-}
rmap :: forall world m r w. (Has world m w, Has world m r)
     => (r -> w) -> SystemT world m ()
rmap f = do
  sr :: Storage r <- getStore
  sc :: Storage w <- getStore
  w <- System ask
  lift$ do
    sl <- explMembers sr
    U.forM_ sl $ \ e -> do
      r <- explGetUnsafe sr e
      explSet sc e (f r)

-- | Maps a function over all entities with a @r@, and writes or deletes their @w@
{-# INLINE rmap' #-}
rmap' :: forall world m r w. (Has world m w, Has world m r, Store m (Storage r), Store m (Storage w))
      => (r -> Safe w) -> SystemT world m ()
rmap' f = do
  sr :: Storage r <- getStore
  sw :: Storage w <- getStore
  lift$ do
    sl <- explMembers sr
    U.forM_ sl $ \ e -> do
      r <- explGetUnsafe sr e
      explSetMaybe sw e (getSafe $ f r)

-- | For all entities with a @w@, this map reads their @r@ and writes their @w@
{-# INLINE wmap #-}
wmap :: forall world m r w. (Has world m w, Has world m r, Store m (Storage r), Store m (Storage w))
     => (Safe r -> w) -> SystemT world m ()
wmap f = do
  sr :: Storage r <- getStore
  sw :: Storage w <- getStore
  lift$ do
    sl <- explMembers sr
    U.forM_ sl $ \ e -> do
      r <- explGet sr e
      explSet sw e (f . Safe $ r)

-- | For all entities with a @w@, this map reads their @r@ and writes or deletes their @w@
{-# INLINE wmap' #-}
wmap' :: forall world m r w. (Has world m w, Has world m r, Store m (Storage r), Store m (Storage w))
      => (Safe r -> Safe w) -> SystemT world m ()
wmap' f = do
  sr :: Storage r <- getStore
  sw :: Storage w <- getStore
  lift$ do
    sl <- explMembers sr
    U.forM_ sl $ \ e -> do
      r <- explGet sr e
      explSetMaybe sw e (getSafe . f . Safe $ r)

-- | Reads a global value
{-# INLINE getGlobal #-}
getGlobal :: forall w m c. (Has w m c, GlobalStore (Storage c)) => SystemT w m c
getGlobal = do s :: Storage c <- getStore
               lift$ explGet s 0

-- | Writes a global value
{-# INLINE setGlobal #-}
setGlobal :: forall w m c. (Has w m c, GlobalStore (Storage c)) => c -> SystemT w m ()
setGlobal c = do s :: Storage c <- getStore
                 lift$ explSet s 0 c

-- | Modifies a global value
{-# INLINE modifyGlobal #-}
modifyGlobal :: forall w m c. (Has w m c, GlobalStore (Storage c)) => (c -> c) -> SystemT w m ()
modifyGlobal f = getGlobal >>= setGlobal . f
