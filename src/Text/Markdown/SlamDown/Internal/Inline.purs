module Text.Markdown.Smolder.Internal.Inline where

import Prelude

import Control.Monad.Reader (ask)
import Data.Either (Either(..))
import Data.Foldable (foldl, intercalate, sequence_)
import Data.List (List, filter)
import Data.Map as Map
import Data.Maybe (fromMaybe, maybe)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.String.Regex (regex, replace)
import Data.String.Regex.Flags (global)
import Data.Traversable (traverse)
import Text.Markdown.Smolder.Internal.Type (ToReaderMarkup)
import Text.Markdown.SlamDown (Inline(..), LinkTarget(..))
import Text.Smolder.HTML as HTML
import Text.Smolder.HTML.Attributes as HA 
import Text.Smolder.Markup (text, (!))
import Text.Smolder.Markup as SM

-- | Convert `List Inline` elements into Smolder Markup
toInlineElements :: forall a e. List (Inline a) -> ToReaderMarkup e
toInlineElements is = sequence_ <$> traverse toInlineElement is

-- | Convert a `Inline` elements into Smolder Markup
toInlineElement :: forall a e. Inline a -> ToReaderMarkup e
toInlineElement il =
  case il of
    (Str s) -> pure $ text s
    (Entity s) -> pure $ text s
    (Space) -> pure $ text " "
    (SoftBreak) -> pure $ text "\n"
    (LineBreak) -> pure HTML.br

    (Emph is) -> (pure <<< HTML.em) =<< toInlineElements is
    (Strong is) -> (pure <<< HTML.strong) =<< toInlineElements is
    (Code e s) -> pure $ HTML.code $ text s
    (Link is (InlineLink url)) -> 
      toInlineElements is >>= 
        (pure <<< (HTML.a ! HA.href url))
    (Link is (ReferenceLink ref)) -> toInlineElements is >>= \el -> do  
      { referenceLinks } <- ask 
      let 
        url = maybe "" (\k -> fromMaybe "" $ Map.lookup k referenceLinks) ref
        urlAttr = HA.href url
      pure $ HTML.a ! urlAttr $ el
      
    (Image is url) -> pure $ HTML.img ! HA.src url ! HA.alt (toInlines is)
    (FormField l r e) -> pure $ SM.empty

-- | Convert `List Inline` elements into plain text
toInlines :: forall a. List (Inline a) -> String
toInlines is = foldl (\str il -> str <> toInline il) "" is

-- | Convert `Inline` elements into plain text
toInline :: forall a. Inline a -> String
toInline il = case il of
  (Str s) -> s
  (Entity s) -> s
  (Space) -> " "
  (SoftBreak) -> "\n"
  (LineBreak) -> "\n"
  _ -> ""

-- | Convert `List Inline` elements into URL friendly text. Used for convert to `id` value
encodeInlines :: forall a. List (Inline a) -> String 
encodeInlines inlines = 
  case (regex "[^\\w -]" global) of
    Left _ -> encoded identity
    Right pattern -> encoded (stripInvalidChars pattern)
  where
    replaceSpaces = replaceAll (Pattern " ") (Replacement "_")
    encoded stripFn = (replaceSpaces <<< intercalate "" 
        <<< filter (_ /= "\n") <<< map (stripFn <<< toInline)) inlines
    stripInvalidChars pattern = replace pattern ""
