{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.GI.Gio.ListModel.SeqStore
  ( SeqStore (..),
    seqStoreNew,
    seqStoreFromList,
    empty,
    replaceList,
    seqStoreLookup,
    getSeq,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.GI.Base.BasicTypes
import Data.GI.Base.Overloading (HasParentTypes, ParentTypes)
import Data.GI.Gio.ListModel.CustomStore (CustomStore (..), CustomStoreImpl (..), customStoreGetPrivate, customStoreNew)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import GI.Gio.Interfaces.ListModel (ListModel, listModelItemsChanged)

-- | Imlementation of 'CustomStore' backed by 'IORef (Seq a)'.
newtype SeqStore a = SeqStore (ManagedPtr (CustomStore (IORef (Seq a)) a))

instance TypedObject (SeqStore a) where
  glibType = glibType @ListModel

instance GObject (SeqStore a)

instance HasParentTypes (SeqStore a)

type instance ParentTypes (SeqStore a) = '[ListModel]

-- | Create a new 'SeqStore' from a given 'Seq'.
seqStoreNew :: MonadIO m => Seq a -> m (SeqStore a)
seqStoreNew list = liftIO $ do
  listRef <- newIORef list
  let getLength = fromIntegral . Seq.length <$> readIORef listRef
      getNthItem n = Seq.lookup (fromIntegral n) <$> readIORef listRef
      con (CustomStore ptr) = SeqStore ptr
  customStoreNew listRef CustomStoreImpl {..} con

-- | Create a new 'SeqStore' from a given list.
seqStoreFromList :: MonadIO m => [a] -> m (SeqStore a)
seqStoreFromList = seqStoreNew . Seq.fromList

-- | Create a new empty 'SeqStore'.
empty :: MonadIO m => m (SeqStore a)
empty = seqStoreNew mempty

-- | Replace all elements in a 'SeqStore' with elements from a list. This causes
-- @itemsChanged@ event to be emitted.
replaceList :: MonadIO m => SeqStore a -> [a] -> m ()
replaceList store@(SeqStore customStorePtr) newList = liftIO $ do
  priv <- customStoreGetPrivate (CustomStore customStorePtr)
  oldSeq <- readIORef priv
  let newSeq = Seq.fromList newList
  writeIORef priv newSeq
  listModelItemsChanged store 0 (fromIntegral $ Seq.length oldSeq) (fromIntegral $ Seq.length newSeq)

-- | Get element at a given position, uses 'Seq.lookup'.
seqStoreLookup :: MonadIO m => SeqStore a -> Int -> m (Maybe a)
seqStoreLookup store n = Seq.lookup n <$> getSeq store

-- | Get the 'Seq' out of the 'SeqStore'.
getSeq :: MonadIO m => SeqStore a -> m (Seq a)
getSeq store@(SeqStore customStorePtr) =
  liftIO $ readIORef =<< customStoreGetPrivate (CustomStore customStorePtr)
