module Stuff where
import Spaz (ActionHandler, Element, wiredEq, defaultSpec, dispatch, modify, state, zoom)
import Spaz.DOM (div, div', text)
import Spaz.Props (onClick)
import Control.Applicative ((*>))
import Data.Lens (Lens', lens)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Prelude (Unit, const, show, ($), (+))
import Spaz.HTML(mount)

type State = { number :: Int }

data Action = ResetCounter

_number :: ∀ r. Lens' { number :: Int | r } Int
_number = lens (_.number) (_ { number = _ })

page :: Element State Action
page = div' [div [onClick $ \_ -> dispatch ResetCounter] [text "hehe"], zoom _number stuff]

stuff :: ∀ act. Element Int act
stuff = state $ \st ->
  div 
    [onClick $ \_ -> liftEffect (log "clicked") *> modify \s -> s + 1]
    [text $ show st]

main :: Effect Unit
main = mount "main" (wiredEq defaultSpec page) handler {number:0}

handler :: ActionHandler State Action
handler ResetCounter = modify (const {number: 0})
