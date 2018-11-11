## Module Text.Markdown.Smolder.Internal.Type

#### `ReferenceLinks`

``` purescript
type ReferenceLinks = Map String String
```

Reference URLs Map, used to lookup URLs from unique name

#### `ToReaderMarkup`

``` purescript
type ToReaderMarkup e = Reader (ToMarkupReaderData e) (Markup e)
```

Reader Monad with `ToMarkupReaderData` 

#### `ToMarkupOptions`

``` purescript
type ToMarkupOptions e = { hideHeadingId :: Boolean, hClasses :: Array (Tuple Int String), olClass :: String, ulClass :: String, pClass :: String, renderFencedCodeBlock :: String -> List String -> Markup e }
```

Convert options, you can set classes into common markups
If you don't want to set `id` in headings, set `hideHeadingId`
If you need to render code block content to another markup templates,
define function `renderFencedCodeBlock` option

#### `ToMarkupReaderData`

``` purescript
type ToMarkupReaderData e = { referenceLinks :: ReferenceLinks, options :: ToMarkupOptions e }
```

Reader Monad Data 


