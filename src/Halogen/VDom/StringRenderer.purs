module Halogen.VDom.StringRenderer
  ( render
  , TagType(..)
  ) where

import Prelude

import Data.Array as A
import Data.Maybe (Maybe(..), maybe)
import Data.String as S
import Data.Tuple (snd)

import Halogen.VDom (VDom(..), ElemName(..), Namespace(..), runGraft)
import Halogen.VDom.StringRenderer.Util (escape)

-- | Type used to determine whether an element can be rendered as self-closing
-- | element, for example, "<br/>".
data TagType
  = NormalTag
  | SelfClosingTag

derive instance eqTagType ∷ Eq TagType
derive instance ordTagType ∷ Ord TagType

-- | Renders a `VDom` tree to a string using the specified tag type scheme,
-- | attribute renderer, and widget renderer.
render
  ∷ ∀ attrs widget
  . (Maybe ElemName → String → String) -- Maybe because root element may be Text
  → (ElemName → TagType)
  → (attrs → String)
  → (widget → String)
  → VDom attrs widget
  → String
render textEscape getTagType renderAttrs renderWidget = go (textEscape Nothing)
  where
  go ∷ (String → String) → VDom attrs widget → String
  go textEscape' = case _ of
    Text s → textEscape' s
    Elem namespace elementName attrs children → renderElement namespace elementName attrs children
    Keyed namespace elementName attrs kchildren → renderElement namespace elementName attrs (map snd kchildren)
    Widget widget → renderWidget widget
    Grafted g → go textEscape' (runGraft g) -- Graft may render Text

  renderElement ∷ (Maybe Namespace) → ElemName → attrs → Array (VDom attrs widget) → String
  renderElement maybeNamespace elemName@(ElemName name) attrs children =
    let
      renderedAttributes ∷ String
      renderedAttributes = renderAttrs attrs

      renderedAttributes' ∷ String
      renderedAttributes' = maybe renderedAttributes (\(Namespace ns) → "xmlns=\"" <> escape ns <> "\"" <> if S.null renderedAttributes then "" else " " <> renderedAttributes) maybeNamespace

      renderedChildren ∷ Array String
      renderedChildren = map (go $ textEscape (Just elemName)) children
    in
      "<" <> name <> (if S.null renderedAttributes then "" else " ") <> renderedAttributes <>
        if A.null children
        then if getTagType elemName == SelfClosingTag then "/>" else "></" <> name <> ">"
        else ">" <> S.joinWith "" renderedChildren <> "</" <> name <> ">"
