{-# LANGUAGE Strict #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Apecs.Stores
  ( Map, Set, Flag(..), Cache, Unique,
    Global,
    Cachable,
  ) where

import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as S
import Data.IORef
import Data.Maybe (fromJust)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Mutable as VM
import Control.Monad.Reader
import GHC.TypeLits
import Data.Proxy

import Apecs.Types

{-# INLINE defaultSetMaybe #-}
defaultSetMaybe :: (Store m s, SafeRW s ~ Maybe (Stores s)) => s -> Int -> Maybe (Stores s) -> m ()
defaultSetMaybe s e Nothing  = explDestroy s e
defaultSetMaybe s e (Just c) = explSet s e c

-- | A map from Data.Intmap.Strict. O(log(n)) for most operations.
--   Yields safe runtime representations of type @Maybe c@.
newtype Map c = Map (IORef (M.IntMap c))
instance MonadIO m => Store m (Map c) where
  type Stores (Map c) = c
  type SafeRW (Map c) = Maybe c
  initStore = liftIO$ Map <$> newIORef mempty
  explDestroy (Map ref) ety = liftIO$ modifyIORef' ref (M.delete ety)
  explMembers (Map ref)     = liftIO$ U.fromList . M.keys <$> readIORef ref
  explExists  (Map ref) ety = liftIO$ M.member ety <$> readIORef ref
  explReset   (Map ref)     = liftIO$ writeIORef ref mempty
  explGetUnsafe (Map ref) ety = liftIO$ fromJust . M.lookup ety <$> readIORef ref
  explGet       (Map ref) ety = liftIO$ M.lookup ety <$> readIORef ref
  explSet       (Map ref) ety x = liftIO$ modifyIORef' ref $ M.insert ety x
  explSetMaybe = defaultSetMaybe
  explModify    (Map ref) ety f = liftIO$ modifyIORef' ref $ M.adjust f ety
  explCmap      (Map ref) f = liftIO$ modifyIORef' ref $ M.map f
  explCmapM_    (Map ref) ma = liftIO (readIORef ref) >>= mapM_ ma
  explCmapM     (Map ref) ma = liftIO (readIORef ref) >>= mapM  ma . M.elems
  explCimapM_   (Map ref) ma = liftIO (readIORef ref) >>= mapM_ ma . M.assocs
  explCimapM    (Map ref) ma = liftIO (readIORef ref) >>= mapM  ma . M.assocs
  {-# INLINE explDestroy #-}
  {-# INLINE explMembers #-}
  {-# INLINE explExists #-}
  {-# INLINE explReset #-}
  {-# INLINE explGetUnsafe #-}
  {-# INLINE explGet #-}
  {-# INLINE explSet #-}
  {-# INLINE explSetMaybe #-}
  {-# INLINE explCmap #-}
  {-# INLINE explModify #-}
  {-# INLINE explCmapM_ #-}
  {-# INLINE explCmapM #-}
  {-# INLINE explCimapM_ #-}
  {-# INLINE explCimapM #-}

-- | Class for flags, used by @Set@ to yield runtime representations.
class Flag c where
  flag :: c

-- | A store that keeps membership, but holds no values.
--   Produces @flag@ runtime values.
newtype Set c = Set (IORef S.IntSet)
instance Flag c => Store IO (Set c) where
  type Stores (Set c) = c
  type SafeRW (Set c) = Bool
  initStore = Set <$> newIORef mempty
  explDestroy (Set ref) ety = modifyIORef' ref (S.delete ety)
  explMembers (Set ref) = U.fromList . S.toList <$> readIORef ref
  explReset (Set ref) = writeIORef ref mempty
  explExists (Set ref) ety = S.member ety <$> readIORef ref
  explImapM_  (Set ref) ma = liftIO (readIORef ref) >>= mapM_ ma . S.toList
  explImapM   (Set ref) ma = liftIO (readIORef ref) >>= mapM  ma . S.toList
  {-# INLINE explDestroy #-}
  {-# INLINE explMembers #-}
  {-# INLINE explExists #-}
  {-# INLINE explReset #-}
  {-# INLINE explImapM_ #-}
  {-# INLINE explImapM #-}

  explGetUnsafe _ _ = return flag
  explGet (Set ref) ety = S.member ety <$> readIORef ref
  explSet (Set ref) ety _ = modifyIORef' ref $ S.insert ety
  explSetMaybe s ety False = explDestroy s ety
  explSetMaybe s ety True  = explSet s ety flag
  explCmap _ _ = return ()
  explModify _ _ _ = return ()
  explCmapM   s m = explImapM  s (m . const flag)
  explCmapM_  s m = explImapM_ s (m . const flag)
  explCimapM  s m = explImapM  s (m . flip  (,) flag)
  explCimapM_ s m = explImapM_ s (m . flip  (,) flag)
  {-# INLINE explGetUnsafe #-}
  {-# INLINE explGet #-}
  {-# INLINE explSet #-}
  {-# INLINE explSetMaybe #-}
  {-# INLINE explCmap #-}
  {-# INLINE explModify #-}

-- | A Unique contains at most one component.
--   Writing to it overwrites both the previous component and its owner.
data Unique c = Unique (IORef Int) (IORef c)
instance Store IO (Unique c) where
  type Stores (Unique c) = c
  type SafeRW (Unique c) = Maybe c
  initStore = Unique <$> newIORef (-1) <*> newIORef undefined
  explDestroy (Unique eref _) ety = do e <- readIORef eref; when (e==ety) (writeIORef eref (-1))

  explMembers (Unique eref _) = f <$> readIORef eref
    where f (-1) = mempty
          f x    = U.singleton x
  explReset   (Unique eref _) = writeIORef eref (-1)
  explExists  (Unique eref _) ety = (==ety) <$> readIORef eref
  explImapM_  (Unique eref _) ma = do e <- liftIO (readIORef eref); when (e /= -1) (void$ ma e)
  explImapM   (Unique eref _) ma = do
    e <- liftIO (readIORef eref)
    if e /= -1 then return [] else pure <$> ma e
  {-# INLINE explDestroy #-}
  {-# INLINE explMembers #-}
  {-# INLINE explExists #-}
  {-# INLINE explReset #-}
  {-# INLINE explImapM_ #-}
  {-# INLINE explImapM #-}

  explGetUnsafe (Unique _ cref) _ = readIORef cref
  explGet       (Unique eref cref) ety = do
    e <- readIORef eref
    if e == ety && e /= -1 then Just <$> readIORef cref else return Nothing

  explSet       (Unique eref cref) ety x = writeIORef eref ety >> writeIORef cref x
  explSetMaybe = defaultSetMaybe
  explCmap      (Unique eref cref) f = readIORef eref >>= \e -> unless (e == -1) $ modifyIORef' cref f
  explModify    (Unique eref cref) ety f = do
    e <- readIORef eref
    when (e==ety && e /= -1) (modifyIORef' cref f)

  explCmapM (Unique eref cref) ma = do
    e <- liftIO$ readIORef eref
    if e /= -1 then liftIO (readIORef cref) >>= fmap pure . ma
               else return []

  explCmapM_ (Unique eref cref) ma = do
    e <- liftIO$ readIORef eref
    when (e /= -1) . void $ liftIO (readIORef cref) >>= ma

  explCimapM (Unique eref cref) ma = do
    e <- liftIO$ readIORef eref
    if e /= -1 then liftIO (readIORef cref) >>= fmap pure . ma . (,) e
               else return []

  explCimapM_ (Unique eref cref) ma = do
    e <- liftIO$ readIORef eref
    when (e /= -1) . void $ liftIO (readIORef cref) >>= ma . (,) e

  {-# INLINE explGetUnsafe #-}
  {-# INLINE explGet #-}
  {-# INLINE explSet #-}
  {-# INLINE explSetMaybe #-}
  {-# INLINE explCmap #-}
  {-# INLINE explModify #-}

-- | Constant value. Not very practical, but fun to write.
--   Contains `mempty`
newtype Const c = Const c
instance (Monad m, Monoid c) => Store m (Const c) where
  type Stores (Const c) = c
  initStore = return$ Const mempty
  explDestroy _ _ = return ()
  explExists  _ _  = return False
  explMembers _ = return mempty
  explReset _ = return ()
  type SafeRW (Const c) = c
  explGetUnsafe (Const c) _ = return c
  explGet       (Const c) _ = return c
  explSet       _ _ _ = return ()
  explSetMaybe  _ _ _ = return ()
  explModify    _ _ _ = return ()
  explCmap       _ _ = return ()
instance (Monoid c) => GlobalStore (Const c) where

-- | Global value.
--   Initialized with 'mempty'
newtype Global c = Global (IORef c)
instance Monoid c => GlobalStore (Global c)
instance Monoid c => Store IO (Global c) where
  type Stores   (Global c) = c
  initStore = Global <$> newIORef mempty

  type SafeRW (Global c) = c
  explDestroy _ _ = return ()
  explExists _ _ = return False
  explGetUnsafe (Global ref) _ = readIORef ref
  explGet (Global ref) _ = readIORef ref
  explSet (Global ref) _ c = writeIORef ref c
  explSetMaybe = explSet
  explMembers = return mempty


-- | A cache around another store.
--   The wrapped store must produce safe representations using Maybe.
--   Note that iterating over a cache is linear in its size, so large, sparsely populated caches might actually decrease performance.
data Cache (n :: Nat) s =
  Cache Int (UM.IOVector Int) (VM.IOVector (Stores s)) s

class (Store IO s, SafeRW s ~ Maybe (Stores s)) => Cachable s
instance Cachable (Map s)
instance (KnownNat n, Cachable s) => Cachable (Cache n s)

instance (KnownNat n, Cachable s) => Store IO (Cache n s) where
  type Stores (Cache n s) = Stores s
  initStore = do
    let n = fromIntegral$ natVal (Proxy @n)
    tags <- UM.replicate n (-1)
    cache <- VM.new n
    child <- initStore
    return (Cache n tags cache child)

  {-# INLINE explDestroy #-}
  explDestroy (Cache n tags _ s) ety = do
    tag <- UM.unsafeRead tags (ety `rem` n)
    if tag == ety
       then UM.unsafeWrite tags (ety `rem` n) (-1)
       else explDestroy s ety

  {-# INLINE explExists #-}
  explExists (Cache n tags _ s) ety = do
    tag <- UM.unsafeRead tags (ety `rem` n)
    if tag == ety then return True else explExists s ety

  {-# INLINE explMembers #-}
  explMembers (Cache _ tags _ s) = do
    cached <- U.filter (/= (-1)) <$> U.freeze tags
    stored <- explMembers s
    return $! cached U.++ stored

  {-# INLINE explReset #-}
  explReset (Cache n tags _ s) = do
    forM_ [0..n-1] $ \e -> UM.write tags e (-1)
    explReset s

  {-# INLINE explImapM_ #-}
  explImapM_ (Cache _ tags _ s) ma = do
    liftIO (U.freeze tags) >>= U.mapM_ ma . U.filter (/= (-1))
    explImapM_ s ma

  {-# INLINE explImapM #-}
  explImapM (Cache _ tags _ s) ma = do
    as1 <- liftIO (U.freeze tags) >>= mapM ma . U.toList . U.filter (/= (-1))
    as2 <- explImapM s ma
    return (as1 ++ as2)

  type SafeRW (Cache n s) = SafeRW s

  {-# INLINE explGetUnsafe #-}
  explGetUnsafe (Cache n tags cache s) ety = do
    let index = ety `rem` n
    tag <- UM.unsafeRead tags index
    if tag == ety
       then VM.unsafeRead cache index
       else explGetUnsafe s ety

  {-# INLINE explGet #-}
  explGet (Cache n tags cache s) ety = do
    let index = ety `rem` n
    tag <- UM.unsafeRead tags index
    if tag == ety
       then Just <$> VM.unsafeRead cache index
       else explGet s ety

  {-# INLINE explSet #-}
  explSet (Cache n tags cache s) ety x = do
    let index = ety `rem` n
    tag <- UM.unsafeRead tags index
    when (tag /= (-1) && tag /= ety) $ do
      cached <- VM.unsafeRead cache index
      explSet s tag cached
    UM.unsafeWrite tags  index ety
    VM.unsafeWrite cache index x

  {-# INLINE explSetMaybe #-}
  explSetMaybe = defaultSetMaybe

  {-# INLINE explCmap #-}
  explCmap (Cache n tags cache s) f = do
    forM_ [0..n-1] $ \e -> do
      tag <- UM.read tags e
      unless (tag == (-1)) (VM.modify cache f e)
    explCmap s f

  {-# INLINE explModify #-}
  explModify (Cache n tags cache s) ety f = do
    let index = ety `rem` n
    tag <- UM.read tags index
    if tag == ety
       then VM.modify cache f ety
       else explModify s ety f

  {-# INLINE explCmapM_ #-}
  explCmapM_ (Cache n tags cache s) ma = do
    forM_ [0..n-1] $ \e -> do
      tag <- liftIO$ UM.read tags e
      unless (tag == (-1)) $ do
        r <- liftIO$ VM.read cache e
        void$ ma r
    explCmapM_ s ma

  {-# INLINE explCimapM_ #-}
  explCimapM_ (Cache n tags cache s) ma = do
    forM_ [0..n-1] $ \e -> do
      tag <- liftIO$ UM.read tags e
      unless (tag == (-1)) $ do
        r <- liftIO$ VM.read cache e
        void$ ma (e, r)
    explCimapM_ s ma
