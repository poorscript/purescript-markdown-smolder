module Text.Markdown.Smolder.Internal.Type where

import Control.Monad.Reader (Reader)
import Data.List (List)
import Data.Map as Map
import Data.Tuple (Tuple)
import Text.Smolder.Markup (Markup)

-- | Reference URLs Map, used to lookup URLs from unique name
type ReferenceLinks = Map.Map String String

-- | Reader Monad with `ToMarkupReaderData` 
type ToReaderMarkup e = Reader (ToMarkupReaderData e) (Markup e)

-- | Convert options, you can set classes into common markups
-- | If you don't want to set `id` in headings, set `hideHeadingId`
-- | If you need to render code block content to another markup templates,
-- | define function `renderFencedCodeBlock` option
type ToMarkupOptions e = 
  { hideHeadingId :: Boolean
  , hClasses :: Array (Tuple Int String)
  , olClass :: String 
  , ulClass :: String
  , pClass :: String 
  , renderFencedCodeBlock :: String -> List String -> Markup e
  }

-- | Reader Monad Data 
type ToMarkupReaderData e = 
  { referenceLinks :: ReferenceLinks 
  , options :: ToMarkupOptions e
  }
