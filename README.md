# gi-gio-hs-list-model

This library implements the `GListModelInterface` which is required for using
`ListView`. The library can be used in two ways: (1) use the provided `SeqStore`
which uses a `Seq` underneath to store the list, or (2) implement functions
required for `CustomStoreImpl` in the way `SeqStore` does.

## Example use of `SeqStore` with `ListView`

```haskell
data Person = Person { name :: Text, age :: Int }

mkListView :: IO Gtk.ListView
mkListView workChan messageViewStore = do
  factory <- new Gtk.SignalListItemFactory [ On #setup createEmptyItem
                                           , On #bind populateItem
                                           ]

  model <- seqStoreNew [ Person "Faizal Khan" 30
                       , Person "Ramadhir Singh" 60
                       ]

  selection <- new Gtk.SingleSelection [#model := model]
  new Gtk.ListView [ #model := selection, #factory := factory]

createEmptyItem :: Gtk.ListItem -> IO ()
createEmptyItem listItem = do
  label <- new Gtk.Label []
  set listItem [#child := label]

populateItem :: Gtk.ListItem -> IO ()
populateItem listItem = do
  item <- fromJust <$> get listItem #item
  storeItem <- fromJust <$> Gtk.castTo CustomStoreItem item
  person <- deRefCustomStoreItem storeItem

  child <- fromJust <$> get convListItem #child
  label <- fromJust <$> Gtk.castTo Gtk.Label child
  set label [ #label := name person <> " " <> Text.pack (show (age person)) ]
```
