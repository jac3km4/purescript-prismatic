module Spaz.HTML where
import React.DOM (IsDynamic(..), mkDOM)
import React.DOM.Props as P
import ReactDOM (render)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)
import Spaz (ActionHandler, Component, Eff, Element, SubId(..), element, interpretEff)
import Effect (Effect)
import Data.Map as Map
import Data.Maybe (fromJust)
import Effect.Aff (launchAff_)
import Effect.Ref as Ref
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, map, pure, unit, (#), ($))

type Props st act = (Eff st act Unit -> Effect Unit) -> P.Props

mkElement
  :: ∀ st act
   . String                   -- | Element name
  -> Array (Props st act)     -- | Props
  -> Array (Element st act)   -- | Children
  -> Element st act
mkElement element props children effect st =
  mkDOM (IsDynamic false) element (map ((#) effect) props) (map (\e -> e effect st) children)

-- | Attach `Element` to the DOM element with the specified id and mount it.
mount
  :: ∀ st act. String     -- | Element id
  -> Component st act     -- | Element
  -> ActionHandler st act -- | Action handler
  -> st                   -- | Initial state
  -> Effect Unit
mount elemId cmp handler st = do
  e <- getHandle
  u <- spec
  _ <- render u e
  pure unit
  where
    spec = do
      model <- Ref.new {root: st, subscriptions: Map.empty, subId: SubId 0 }
      let root = { model, handler }
          eff = \effect -> launchAff_ $ interpretEff root effect
      pure $ element cmp eff st
    getHandle = do
      win <- window
      doc <- document win
      e   <- getElementById elemId $ toNonElementParentNode doc
      pure $ unsafePartial $ fromJust e
