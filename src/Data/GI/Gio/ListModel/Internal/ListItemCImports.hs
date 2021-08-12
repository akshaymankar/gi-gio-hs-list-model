module Data.GI.Gio.ListModel.Internal.ListItemCImports where

import Data.GI.Base (CGType)
import Foreign (Ptr, StablePtr)

foreign import ccall "GiGioHsListItem.h gi_gio_hs_list_item_new"
  giGioHsListItemNew :: StablePtr a -> IO (Ptr ())

foreign import ccall "GiGioHsListItem.h gi_gio_hs_list_item_get_type"
  listItemGetType :: IO CGType
