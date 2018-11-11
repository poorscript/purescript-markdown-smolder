## Module Text.Markdown.Smolder.Internal.Inline

#### `toInlineElements`

``` purescript
toInlineElements :: forall a e. List (Inline a) -> ToReaderMarkup e
```

Convert `List Inline` elements into Smolder Markup

#### `toInlineElement`

``` purescript
toInlineElement :: forall a e. Inline a -> ToReaderMarkup e
```

Convert a `Inline` elements into Smolder Markup

#### `toInlines`

``` purescript
toInlines :: forall a. List (Inline a) -> String
```

Convert `List Inline` elements into plain text

#### `toInline`

``` purescript
toInline :: forall a. Inline a -> String
```

Convert `Inline` elements into plain text

#### `encodeInlines`

``` purescript
encodeInlines :: forall a. List (Inline a) -> String
```

Convert `List Inline` elements into URL friendly text. Used for convert to `id` value


