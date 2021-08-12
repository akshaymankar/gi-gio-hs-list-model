{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.GI.Gio.ListModel.CustomStoreItem
  ( CustomStoreItem (..),
    customStoreItemGetItem,
    fromObject,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.GI.Base (castTo, withManagedPtr)
import Data.GI.Base.BasicTypes
import Data.GI.Base.Overloading (HasParentTypes, ParentTypes)
import Data.GI.Gio.ListModel.Internal.ListItemCImports
import Foreign (Ptr, StablePtr, deRefStablePtr)
import GI.GObject (Object)

foreign import ccall "GiGioHsListItem.h gi_gio_hs_list_item_get_item"
  giGioHsListItemGetItem :: Ptr (CustomStoreItem a) -> IO (StablePtr a)

-- | The 'Object' returned by 'GI.Gtk.Objects.ListItem.listItemGetItem' can be
-- cast into tihs type. 'fromObject' can be used for convinience.
newtype CustomStoreItem a = CustomStoreItem (ManagedPtr (CustomStoreItem a))

instance HasParentTypes (CustomStoreItem a)

type instance ParentTypes (CustomStoreItem a) = '[Object]

instance TypedObject (CustomStoreItem a) where
  glibType = GType <$> listItemGetType

instance GObject (CustomStoreItem a)

customStoreItemGetItem :: MonadIO m => CustomStoreItem a -> m a
customStoreItemGetItem item =
  liftIO $ withManagedPtr item giGioHsListItemGetItem >>= deRefStablePtr

fromObject :: forall a m. MonadIO m => Object -> m (Maybe a)
fromObject obj = liftIO $ do
  mStoreItem :: Maybe (CustomStoreItem a) <- castTo CustomStoreItem obj
  traverse customStoreItemGetItem mStoreItem
