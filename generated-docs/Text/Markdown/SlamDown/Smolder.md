## Module Text.Markdown.SlamDown.Smolder

#### `defaultToMarkupOptions`

``` purescript
defaultToMarkupOptions :: forall e. ToMarkupOptions e
```

Default convert options

#### `toMarkup`

``` purescript
toMarkup :: forall a e. SlamDownP a -> Markup e
```

Convert Smolder markup from Slamdown Markdown without options

#### `toMarkup'`

``` purescript
toMarkup' :: forall a e. ToMarkupOptions e -> SlamDownP a -> Markup e
```

Convert Smolder markup from Slamdown Markdown with options


### Re-exported from Text.Markdown.Smolder.Internal.Block:

#### `toElements`

``` purescript
toElements :: forall a e. List (Block a) -> ToReaderMarkup e
```

Convert Markdown block elements to Smolder markup

#### `toElement`

``` purescript
toElement :: forall a e. Block a -> ToReaderMarkup e
```

Convert Markdown block element to Smolder markup

#### `renderFencedCodeBlock`

``` purescript
renderFencedCodeBlock :: forall e. String -> List String -> Markup e
```

Default render fenced code block. You can render yourself set it in options

### Re-exported from Text.Markdown.Smolder.Internal.Type:

#### `ToReaderMarkup`

``` purescript
type ToReaderMarkup e = Reader (ToMarkupReaderData e) (Markup e)
```

Reader Monad with `ToMarkupReaderData` 

#### `ToMarkupReaderData`

``` purescript
type ToMarkupReaderData e = { referenceLinks :: ReferenceLinks, options :: ToMarkupOptions e }
```

Reader Monad Data 

#### `ToMarkupOptions`

``` purescript
type ToMarkupOptions e = { hideHeadingId :: Boolean, hClasses :: Array (Tuple Int String), olClass :: String, ulClass :: String, pClass :: String, renderFencedCodeBlock :: String -> List String -> Markup e }
```

Convert options, you can set classes into common markups
If you don't want to set `id` in headings, set `hideHeadingId`
If you need to render code block content to another markup templates,
define function `renderFencedCodeBlock` option

#### `ReferenceLinks`

``` purescript
type ReferenceLinks = Map String String
```

Reference URLs Map, used to lookup URLs from unique name

