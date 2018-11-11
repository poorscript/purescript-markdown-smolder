module Text.Markdown.SlamDown.Smolder 
  ( defaultToMarkupOptions
  , toMarkup
  , toMarkup'
  , module Block
  , module Type
  ) where

import Prelude

import Control.Monad.Reader (runReader)
import Data.Foldable (foldMap, foldl)
import Data.List ((:))
import Data.Map (unions)
import Data.Map as Map
import Text.Markdown.SlamDown (Block(..), SlamDownP(..))
import Text.Markdown.Smolder.Internal.Block (renderFencedCodeBlock, toElements, toElement) as Block
import Text.Markdown.Smolder.Internal.Type (ToMarkupOptions, ReferenceLinks)
import Text.Markdown.Smolder.Internal.Type (ReferenceLinks, ToMarkupOptions, ToMarkupReaderData, ToReaderMarkup) as Type
import Text.Smolder.Markup (Markup)


-- | Default convert options
defaultToMarkupOptions :: forall e. ToMarkupOptions e
defaultToMarkupOptions = 
  { hideHeadingId: false
  , renderFencedCodeBlock: Block.renderFencedCodeBlock
  , olClass: ""
  , ulClass: ""
  , hClasses: []
  , pClass: ""
  }

-- | Convert Smolder markup from Slamdown Markdown without options
toMarkup :: forall a e. SlamDownP a -> Markup e
toMarkup = toMarkup' defaultToMarkupOptions

-- | Convert Smolder markup from Slamdown Markdown with options
toMarkup' :: forall a e. ToMarkupOptions e -> SlamDownP a -> Markup e 
toMarkup' options (SlamDown bs) = runReader (Block.toElements bs) 
  { referenceLinks 
  , options
  }  
  where 
    referenceLinks = foldMap getBlockLinkRefs bs

-- Get all reference links from markdown
getBlockLinkRefs :: forall a. Block a -> ReferenceLinks
getBlockLinkRefs = getBlockLinkRefs' Map.empty
  where
    getBlockLinkRefs' links block = 
      case block of 
        (LinkReference k url) -> Map.insert k url links
        (Blockquote bs) -> foldl getBlockLinkRefs' links bs
        (Lst _ bss) -> unions $ links : map (foldl getBlockLinkRefs' Map.empty) bss
        _ -> links

