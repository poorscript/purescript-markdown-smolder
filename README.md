# purescript-markdown-smolder

[![Pursuit](https://pursuit.purescript.org/packages/purescript-markdown-smolder/badge)](https://pursuit.purescript.org/hgiasac/purescript-markdown-smolder)
[![ComVer](https://img.shields.io/badge/ComVer-compliant-brightgreen.svg)](https://github.com/staltz/comver)
[![Build Status](https://travis-ci.org/hgiasac/purescript-markdown-smolder.svg?branch=master)](https://travis-ci.org/hgiasac/purescript-markdown-smolder)

This library is forked from [original one][3], with up-to-date dependencies and breaking changes fixed. I also add simple customized options:

* HTML class for markups `ul`, `ol`, `p`, `h1`, `h2`,...
* Customized code block renderer 

## Example 

``` purescript

compileMd' :: forall e. ToMarkupOptions e -> String -> String 
compileMd' options input =
  either identity (toMarkup' options >>> render)
  (parseMd input :: Either String (SlamDownP String))

compileMd :: String -> String
compileMd = compileMd' defaultToMarkupOptions

compileMd "# Hello"
-- <h1 id=\"Hello\">Hello</h1>

compileMd' (defaultToMarkupOptions { hideHeadingId = true }) "# Hello"
-- <h1>Hello</h1>

```
## Quick Reference

### Module Text.Markdown.SlamDown.Smolder

#### `ToMarkupOptions`

``` purescript
type ToMarkupOptions e = 
  { hideHeadingId :: Boolean
  , hClasses :: Array (Tuple Int String)
  , olClass :: String
  , ulClass :: String
  , pClass :: String
  , renderFencedCodeBlock :: String -> List String -> Markup e 
  }
```

Convert options, you can set classes into common markups
If you don't want to set `id` in headings, set `hideHeadingId`
If you need to render code block content to another markup templates,
define function `renderFencedCodeBlock` option

#### `defaultToMarkupOptions`

Default convert options

``` purescript
defaultToMarkupOptions :: forall e. ToMarkupOptions e

```


#### `toMarkup`

``` purescript
toMarkup :: forall a e. SlamDownP a -> Markup e
```

Render [purescript-markdown][1] to [purescript-smolder][2], with default options.

#### `toMarkup'`

``` purescript
toMarkup' :: forall a e. ToMarkupOptions e -> SlamDownP a -> Markup e
```

Convert Smolder markup from Slamdown Markdown with options



[1]: https://pursuit.purescript.org/packages/purescript-markdown
[2]: https://pursuit.purescript.org/packages/purescript-smolder
[3]: https://github.com/alexmingoia/purescript-markdown-smolder
