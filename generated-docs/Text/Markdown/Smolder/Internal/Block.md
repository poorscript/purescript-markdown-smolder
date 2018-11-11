## Module Text.Markdown.Smolder.Internal.Block

#### `toElement`

``` purescript
toElement :: forall a e. Block a -> ToReaderMarkup e
```

Convert Markdown block element to Smolder markup

#### `toElements`

``` purescript
toElements :: forall a e. List (Block a) -> ToReaderMarkup e
```

Convert Markdown block elements to Smolder markup

#### `renderFencedCodeBlock`

``` purescript
renderFencedCodeBlock :: forall e. String -> List String -> Markup e
```

Default render fenced code block. You can render yourself set it in options


