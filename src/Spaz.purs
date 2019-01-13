module Spaz
  ( Eff
  , EffF(..)
  , Subscription
  , SubId(..)
  , InternalProps
  , Interpret
  , PerformAction
  , Component
  , Element
  , modify
  , dispatch
  , defaultSpec
  , defaultEqSpec
  , focus
  , focusS
  , match
  , split
  , foreach
  , withState
  , withStateL
  , element
  , wired
  , stateless
  , runEff
  , noAction
  ) where
import Prelude

import Control.Monad.Free.Trans (FreeT, interpret, liftFreeT, runFreeT)
import Data.Array (index, unsafeIndex, updateAt)
import Data.Either (Either(..), either)
import Data.Foldable (traverse_)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Lens (Lens', Prism', lens, matching, over, prism', review, view, (^.))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Partial.Unsafe (unsafePartial)
import React (ReactClass, ReactElement)
import React as React

type Model st act = { root :: st, subscriptions :: Map SubId (Subscription st act), subId :: SubId }
type ModelRef st act = Ref (Model st act)

type Interpret st act = Eff st act Unit -> Effect Unit
type InternalProps st act = { interp :: Interpret st act, state :: st }

type Element st act = Interpret st act -> st -> ReactElement
type Component st act = ReactClass (InternalProps st act)

type Render st act = st -> Element st act
type PerformAction st act = act -> Eff st act Unit
type ShouldUpdate st = st -> st -> Boolean

type Spec st act =
  { name :: String
  , render :: Render st act
  , performAction :: PerformAction st act
  , componentDidMount :: Eff st act Unit
  , componentWillUnmount :: Eff st act Unit
  , componentDidUpdate :: st -> Eff st act Unit
  , shouldComponentUpdate :: ShouldUpdate st
  }

type Subscription st act =
  { onStateChange :: st -> Aff Unit
  , onAction :: act -> Effect Unit
  }

newtype SubId = SubId Int

derive newtype instance eqSubId :: Eq SubId
derive newtype instance ordSubId :: Ord SubId

type Eff st act = FreeT (EffF st act) Aff

data EffF st act next
  = Dispatch act next
  | Modify (st -> st) next
  | Subscribe (Subscription st act) (SubId -> next)
  | Unsubscribe SubId next

derive instance functorEffectF :: Functor (EffF st act)

runEff :: ∀ st act a. ModelRef st act -> Eff st act a -> Aff a
runEff model = runFreeT eval
  where
    eval :: EffF st act (Eff st act a) -> Aff (Eff st act a) 
    eval (Dispatch act next) = liftEffect $ do
      {subscriptions} <- Ref.read model
      next <$ traverse_ (\fn -> fn.onAction act) subscriptions
    eval (Modify f next) = do
      {root, subscriptions} <- liftEffect $ Ref.modify (\st -> st {root = f st.root}) model
      next <$ traverse_ (\fn -> fn.onStateChange root) subscriptions
    eval (Subscribe sub next) = liftEffect $ do
      let update {root, subscriptions, subId} =
            let subId' = nextSubId subId
                subscriptions' = Map.insert subId' sub subscriptions
            in {root, subscriptions: subscriptions', subId: subId'}
      {subId} <- Ref.modify update model
      pure (next subId)
    eval (Unsubscribe toDelete next) = liftEffect $ do
      let update {root, subscriptions, subId} =
            {root, subscriptions: Map.delete toDelete subscriptions, subId}
      next <$ Ref.modify_ update model
    nextSubId (SubId v) = SubId $ v + 1

-- | Modify a part of the state and notify all the listeners.
modify :: ∀ st act. (st -> st) -> Eff st act Unit
modify f = liftFreeT $ Modify f unit

-- | Dispatch an action to be received by a top level handler.
dispatch :: ∀ st act. act -> Eff st act Unit
dispatch act = liftFreeT $ Dispatch act unit

-- | Create a subscription for a part of the state.
subscribe :: ∀ st act. Subscription st act -> Eff st act SubId
subscribe sub = liftFreeT $ Subscribe sub identity

-- | Remove a subscription by subscription ID.
unsubscribe :: ∀ st act. SubId -> Eff st act Unit
unsubscribe subId = liftFreeT $ Unsubscribe subId unit

-- | Modify `Element` substate specified by a `Lens`.
modifyL :: ∀ st stt act. Lens' st stt -> (stt -> stt) -> Eff st act Unit
modifyL lns = modify <<< over lns

noAction :: ∀ st act. PerformAction st act
noAction _ = pure unit

defaultSpec :: ∀ st act. Render st act -> PerformAction st act -> Spec st act
defaultSpec render performAction =
  { name: ""
  , render
  , performAction
  , componentDidMount: pure unit
  , componentWillUnmount: pure unit
  , componentDidUpdate: const $ pure unit
  , shouldComponentUpdate: \_ _ -> true
  }

defaultEqSpec :: ∀ st act. (Eq st) => Render st act -> PerformAction st act -> Spec st act
defaultEqSpec render performAction = spec { shouldComponentUpdate = (/=) }
  where spec = defaultSpec render performAction

-- | Create a `Component` from a `Spec`.
-- | This function subscribes the component to actions and all state updates.
-- | Each update will trigger an equality check and possibly a rerender.
-- | You should avoid nesting those components because each of them has a separate subscription.
wired :: ∀ st act. Spec st act -> Component st act
wired spec = React.component spec.name constructor
  where
    constructor this = do
      subscriptionRef <- Ref.new $ pure unit
      initial <- React.getProps this
      let initialState = initial.state
      pure $
        { render: do
            {interp} <- React.getProps this
            {state} <- React.getState this
            pure $ spec.render state interp state
        , state: {state: initialState}
        , componentDidMount: do
            {interp} <- React.getProps this
            interp $ createSubscription this subscriptionRef (interp <$> spec.performAction) *> spec.componentDidMount
        , shouldComponentUpdate: \_ {state} -> do
            current <- React.getState this
            pure $ spec.shouldComponentUpdate current.state state
        , componentDidUpdate: \{interp, state} _ _ ->
            interp $ spec.componentDidUpdate state
        , componentWillUnmount: do
            {interp} <- React.getProps this
            unsub <- Ref.read subscriptionRef
            interp $ unsub *> spec.componentWillUnmount
        }
    createSubscription this ref onAction = do
      subId <- subscribe {onStateChange: performUpdate this, onAction }
      liftEffect $ Ref.write (unsubscribe subId) ref
    performUpdate this state = do
      old <- liftEffect $ React.getState this
      if spec.shouldComponentUpdate old.state state
        then void $ makeAff \cb ->
          nonCanceler <$ React.writeStateWithCallback this {state: state} (cb $ Right state)
        else pure unit

-- | Shortcut for creating React stateless components.
stateless :: ∀ st act. Element st act -> Component st act
stateless render = React.statelessComponent $ \{interp, state} -> render interp state

-- | Create a `Element` from a `Component`.
element :: ∀ st act. Component st act -> Element st act
element class_ interp state = React.createLeafElement class_ {interp, state}

-- | Reify the current `Element` state.
withState :: ∀ st act. (st -> Element st act) -> Element st act
withState f interp st = (f st) interp st

-- | Reify `Element` substate specified by a lens.
withStateL :: ∀ st stt act. Lens' st stt -> (stt -> Element st act) -> Element st act
withStateL lns f interp st = (f (st ^. lns)) interp st

-- | Embed a `Element` with a state of type `stt` into a `Element`
-- | with a state of type `st`.
focus
  :: ∀ state1 state2 action1 action2
   . Lens' state2 state1
  -> Prism' action2 action1
  -> Element state1 action1
  -> Element state2 action2
focus lens prism el interp state = el (interp <<< focusEff lens prism) (state ^. lens)

-- | A variant of `focus` which only changes the state type, by applying a `Lens`.
focusS
  :: ∀ state1 state2 action
   . Lens' state2 state1
  -> Element state1 action
  -> Element state2 action
focusS lens = focus lens identity

-- | A variant of `focus` which only changes the action type, by applying a `Prism`,
-- | effectively matching some subset of a larger action type.
match
  :: ∀ state action1 action2
   . Prism' action2 action1
  -> Element state action1
  -> Element state action2
match prism = focus identity prism

-- | Create an element which renders an optional subelement.
split
  :: forall state1 state2 action
   . Prism' state1 state2
  -> Element state2 action
  -> Element state1 action
split prism el interp =
  either (const mempty) (el (interp <<< interpret splitEff)) <<< matching prism
  where
    splitEff :: ∀ next. EffF state2 action next -> EffF state1 action next
    splitEff (Dispatch act next) = Dispatch act next
    splitEff (Modify f next) =
      let apply st = either (const st) (review prism <<< f) $ matching prism st 
      in Modify apply next
    splitEff (Subscribe {onStateChange, onAction} next) =
      let onStateChange' = either (const $ pure unit) onStateChange <<< matching prism
      in Subscribe {onStateChange: onStateChange', onAction} next
    splitEff (Unsubscribe subId next) = Unsubscribe subId next

-- | Create a component whose state is described by a list, displaying one subcomponent
-- | for each entry in the list.
-- |
-- | The action type is modified to take the index of the originating subcomponent as an
-- | additional argument.
foreach
  :: ∀ state action
   . (Int -> Element state action)
  -> Element (Array state) (Tuple Int action)
foreach f interp state = foldMapWithIndex folder state
  where
    folder i a = f i (interp <<< focusEff (lensAtA i) (matcherA i)) (unsafePartial $ unsafeIndex state i)
    matcherA :: ∀ a. Int -> Prism' (Tuple Int a) a
    matcherA i = prism' (\v -> Tuple i v) (\(Tuple i' v) -> if i' == i then Just v else Nothing)
    lensAtA :: ∀ a. Int -> Lens' (Array a) a
    lensAtA i = lens (\m -> unsafePartial $ fromJust $ index m i) (\m v -> fromMaybe m $ updateAt i v m)

focusEff
  :: ∀ state1 state2 action1 action2 a
   . Lens' state2 state1
  -> Prism' action2 action1
  -> Eff state1 action1 a
  -> Eff state2 action2 a
focusEff lens prism = interpret focusEff' 
  where
    focusEff' :: ∀ next. EffF state1 action1 next -> EffF state2 action2 next
    focusEff' (Dispatch act next) = Dispatch (review prism act) next
    focusEff' (Modify f next) = Modify (over lens f) next
    focusEff' (Subscribe sub next) =
      let onStateChange st = sub.onStateChange (view lens st)
          onAction = either (const $ pure unit) sub.onAction <<< matching prism
      in Subscribe {onStateChange, onAction} next
    focusEff' (Unsubscribe subId next) = Unsubscribe subId next
