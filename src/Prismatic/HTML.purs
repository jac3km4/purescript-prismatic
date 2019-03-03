module Prismatic.HTML where
import Data.Map as Map
import Data.Maybe (fromJust)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Ref as Ref
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, join, map, pure, void, (#), ($), (<$>), (<*>), (<<<), (=<<))
import React.DOM (IsDynamic(..), mkDOM)
import ReactDOM as ReactDOM
import Prismatic (Element, SubId(..), runEff)
import Prismatic.VDOM.Props (Props)
import Web.DOM.NonElementParentNode (getElementById) as Web
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument (toNonElementParentNode) as Web
import Web.HTML.Window (document) as Web

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
  -> Element st act       -- | Element
  -> st                   -- | Initial state
  -> Effect Unit
mount elemId cmp st = void $ join $ ReactDOM.render <$> spec <*> handle
  where
    spec = do
      model <- Ref.new {root: st, subscriptions: Map.empty, subId: SubId 0 }
      let eff = launchAff_ <<< runEff model
      pure $ cmp eff st
    handle = do
      elem <- Web.getElementById elemId =<< liftF Web.toNonElementParentNode =<< Web.document =<< Web.window
      pure $ unsafePartial $ fromJust elem
    liftF = map pure
