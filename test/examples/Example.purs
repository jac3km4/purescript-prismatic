module Example where
import Control.Applicative ((*>))
import Data.Either (Either)
import Data.Lens (Lens', _Left, lens)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Prelude (Unit, const, show, ($), (+))
import Spaz as S
import Spaz.HTML (mount)
import Spaz.VDOM (button, div, div', text)
import Spaz.VDOM.Props (onClick)

type State = { number :: Int }

data ResetCounter = ResetCounter

type Action = Either ResetCounter Unit

_number :: ∀ r. Lens' { number :: Int | r } Int
_number = lens (_.number) (_ { number = _ })

page :: S.Element State Action
page = div' [ S.focus _number _Left $ S.element stuff ]

stuff :: S.Component Int ResetCounter
stuff = S.wired $ S.defaultEqSpec render performAction
  where
    render st =
      div' [ button [ onClick $ \_ -> liftEffect (log "clicked") *> S.modify \s -> s + 1] [ text "lel" ]
           , div [onClick $ \_ -> S.dispatch ResetCounter] [text $ show st]
           ]
    performAction ResetCounter = S.modify $ const 0

main :: Effect Unit
main = mount "main" page {number:0}
