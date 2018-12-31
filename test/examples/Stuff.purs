module Stuff where
import Control.Applicative ((*>))
import Data.Lens (Lens', lens)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Prelude (Unit, const, show, ($), (+), (<<<))
import Spaz (ActionHandler, Component, Element, defaultSpec, dispatch, element, foreachZ, modify, state, wiredEq, zoom)
import Spaz.DOM (div, div', text)
import Spaz.HTML (mount)
import Spaz.Props (onClick)

type State = { number :: Int, items :: Array Int }

data Action = ResetCounter

_number :: ∀ r. Lens' { number :: Int | r } Int
_number = lens (_.number) (_ { number = _ })

_items :: ∀ r. Lens' { items :: (Array Int) | r } (Array Int)
_items = lens (_.items) (_ { items = _ })

page :: Element State Action
page = div' [ div [onClick $ \_ -> dispatch ResetCounter] [text "hehe"]
            , zoom _number $ element stuff
            , foreachZ _items $ state $ text <<< show
            ]

stuff :: ∀ act. Component Int act
stuff = wiredEq defaultSpec $ \st ->
  div 
    [onClick $ \_ -> liftEffect (log "clicked") *> modify \s -> s + 1]
    [text $ show st]

main :: Effect Unit
main = mount "main" page handler {number:0, items: [2]}

handler :: ActionHandler State Action
handler ResetCounter = modify (const {number: 0, items: []})
