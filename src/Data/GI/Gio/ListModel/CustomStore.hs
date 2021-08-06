{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Allows a custom data structure to be used with ListView introduced in GTK4.
-- The model 'Data.GI.Gio.ListModel.SeqStore.SeqStore' is based on
-- 'CustomStore'. This module should only be required for implementing a new
-- type of store.
--
-- The users of a 'CustomStore' will need to handle the 'GI.GObject.Object' that is
-- returned by 'GI.Gtk.ListItem.listItemGetItem', for this
-- "Data.GI.Gio.ListModel.CustomStoreItem" can be used.
module Data.GI.Gio.ListModel.CustomStore
  ( CustomStore (..),
    customStoreNew,
    customStoreGetPrivate,
    CustomStoreImpl (..),
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.GI.Base (newObject, withManagedPtr)
import Data.GI.Base.BasicTypes
import Data.GI.Base.Overloading (HasParentTypes, ParentTypes)
import Data.GI.Gio.ListModel.Internal.ListItemCImports (giGioHsListItemNew, listItemGetType)
import Data.Kind (Type)
import Data.Word (Word16)
import Foreign.C.Types
import Foreign.Ptr
import Foreign.StablePtr
import GI.Gio.Interfaces.ListModel (ListModel)

-- C Imports
foreign import ccall "GiGioHsListStore.h gi_gio_hs_list_store_new"
  giGioHsListStoreNew ::
    StablePtr (CustomStoreImpl model a) ->
    StablePtr private ->
    IO (Ptr (CustomStore private a))

foreign import ccall "giGioHsListStore.hs gi_gio_hs_list_store_get_priv"
  giGioHsListStoreGetPriv :: Ptr (CustomStore private a) -> IO (StablePtr private)

-- | A 'CustomStore' is an instance of 'GListModel'. This can be used for any
-- widget that stores 'GListModel'. The user may either create an instance using
-- 'customStoreNew' or use the predefined model
-- 'Data.GI.Gio.ListModel.SeqStore'.
--
-- The 'private' type can be used to represent any data that might be needed to
-- perform operations on the implmentation of 'CustomStore'. For example, this
-- can be an 'Data.IORef.IORef' which can be manipulated as the list needs
-- manipulating.
newtype CustomStore private a = CustomStore (ManagedPtr (CustomStore private a))

instance HasParentTypes (CustomStore private row)

type instance ParentTypes (CustomStore private row) = '[ListModel]

instance TypedObject (CustomStore private a) where
  glibType = glibType @ListModel

instance GObject (CustomStore private a)

-- | Operations required to be defined for any implementation of 'CustomStore'.
data CustomStoreImpl (model :: Type -> Type) a = CustomStoreImpl
  { getLength :: IO Word16,
    getNthItem :: Word16 -> IO (Maybe a)
  }

-- | Create a new instance of a 'CustomStore'.
customStoreNew :: MonadIO m => private -> CustomStoreImpl model a -> (CustomStore private a -> model a) -> m (model a)
customStoreNew private impl con = liftIO $ do
  privPtr <- newStablePtr private
  implPtr <- newStablePtr impl
  storePtr <- giGioHsListStoreNew implPtr privPtr
  con <$> newObject CustomStore storePtr

customStoreGetPrivate :: MonadIO m => CustomStore private a -> m private
customStoreGetPrivate store =
  liftIO $ withManagedPtr store giGioHsListStoreGetPriv >>= deRefStablePtr

-- C Exports

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

listModelGetNItems_static :: StablePtr (CustomStoreImpl model a) -> IO CUInt
listModelGetNItems_static storePtr = do
  impl <- deRefStablePtr storePtr
  fmap fromIntegral . getLength $ impl

foreign export ccall "gi_gio_hs_list_store_get_n_items_impl"
  listModelGetNItems_static :: StablePtr (CustomStoreImpl model a) -> IO CUInt

listModelGetItem_static :: StablePtr (CustomStoreImpl model a) -> CUInt -> IO (Ptr ())
listModelGetItem_static storePtr cpos = do
  impl <- deRefStablePtr storePtr
  let pos = fromIntegral cpos
  maybeItem <- getNthItem impl pos
  case maybeItem of
    Just item -> giGioHsListItemNew =<< newStablePtr item
    Nothing -> pure nullPtr

foreign export ccall "gi_gio_hs_list_store_get_item_impl"
  listModelGetItem_static :: StablePtr (CustomStoreImpl model a) -> CUInt -> IO (Ptr ())

listModelGetItemType_static :: StablePtr (CustomStoreImpl model a) -> IO CGType
listModelGetItemType_static _ = listItemGetType

foreign export ccall "gi_gio_hs_list_store_get_item_type_impl"
  listModelGetItemType_static :: StablePtr (CustomStoreImpl model a) -> IO CGType
