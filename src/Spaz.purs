module Spaz
  ( Eff
  , EffF(..)
  , Subscription
  , SubId(..)
  , ComponentParams
  , ActionHandler
  , Component
  , Element
  , modify
  , dispatch
  , defaultSpec
  , zoom
  , state
  , stateL
  , foreach
  , foreachF
  , foreachZ
  , foreachZF
  , element
  , wired
  , wiredEq
  , wiredL
  , wiredLEq
  , stateless
  , interpretEff
  ) where
import Prelude

import Control.Monad.Free.Trans (FreeT, interpret, liftFreeT, runFreeT)
import Data.Array (catMaybes, index, length, updateAt)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Lens (ALens', Lens', cloneLens, lens, over, view, (^.))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Partial.Unsafe (unsafePartial)
import React (ReactClass, ReactElement, fragmentWithKey, statelessComponent)
import React as React

type Model st act = { root :: st, subscriptions :: Map SubId (Subscription st), subId :: SubId }
type Root st act = { model :: Ref (Model st act), handler :: ActionHandler st act }

type Element st act = (Eff st act Unit -> Effect Unit) -> st -> ReactElement
type Component st act = ReactClass (ComponentParams st act)

type Spec st act =
  { displayName :: String
  , componentDidMount :: Eff st act Unit
  , componentWillUnmount :: Eff st act Unit
  , componentDidUpdate :: st -> Eff st act Unit
  }

newtype SubId = SubId Int

derive newtype instance eqSubId :: Eq SubId
derive newtype instance ordSubId :: Ord SubId

type Subscription st = st -> st -> Aff Unit
type ActionHandler st act = act -> Eff st act Unit
type ComponentParams st act = { effect :: Eff st act Unit -> Effect Unit, state :: st }

type Eff st act = FreeT (EffF st act) Aff

data EffF st act next
  = Dispatch act next
  | Modify (st -> st) next
  | Subscribe (Subscription st) (SubId -> next)
  | Unsubscribe SubId next

derive instance functorEffectF :: Functor (EffF st act)

zoomEffF :: ∀ st stt act next. Lens' st stt -> EffF stt act next -> EffF st act next
zoomEffF lns (Dispatch act next) = Dispatch act next
zoomEffF lns (Modify f next) = Modify (over lns f) next
zoomEffF lns (Subscribe f next) = Subscribe (\a b -> f (view lns a) (view lns b)) next
zoomEffF lns (Unsubscribe subId next) = Unsubscribe subId next

zoomEff :: ∀ st stt act a. Lens' st stt -> Eff stt act a -> Eff st act a
zoomEff lns = interpret (zoomEffF lns)

interpretEff :: ∀ st act a. Root st act -> Eff st act a -> Aff a
interpretEff {model, handler} m = runFreeT eval m
  where
    eval :: EffF st act (Eff st act a) -> Aff (Eff st act a) 
    eval (Dispatch act next) = pure $ handler act *> next
    eval (Modify f next) = do
      {root, subscriptions, subId} <- liftEffect $ Ref.read model
      let root' = f root
      liftEffect $ Ref.write {root: root', subscriptions, subId} model
      traverse_ (\fn -> fn root root') subscriptions
      pure next
    eval (Subscribe sub next) = liftEffect $ do
      let update = \{root, subscriptions, subId} ->
        let subId' = nextSubId subId
            subscriptions' = Map.insert subId' sub subscriptions
            in {root, subscriptions: subscriptions', subId: subId'}
      {subId} <- Ref.modify update model
      pure (next subId)
    eval (Unsubscribe toDelete next) = liftEffect $ do
      let update = \{root, subscriptions, subId} ->
          {root, subscriptions: Map.delete toDelete subscriptions, subId}
      Ref.modify_ update model
      pure next
    nextSubId (SubId v) = SubId $ v + 1

-- | Modify a part of the state and notify all the listeners.
modify :: ∀ st act. (st -> st) -> Eff st act Unit
modify f = liftFreeT $ Modify f unit

-- | Dispatch an action to be received by a top level handler.
dispatch :: ∀ st act. act -> Eff st act Unit
dispatch act = liftFreeT $ Dispatch act unit

-- | Create a subscription for a part of the state.
subscribe :: ∀ st act. Subscription st -> Eff st act SubId
subscribe sub = liftFreeT $ Subscribe sub identity

-- | Remove a subscription by subscription ID.
unsubscribe :: ∀ st act. SubId -> Eff st act Unit
unsubscribe subId = liftFreeT $ Unsubscribe subId unit

defaultSpec :: ∀ st act. Spec st act
defaultSpec =
  { displayName: ""
  , componentDidMount: pure unit
  , componentWillUnmount: pure unit
  , componentDidUpdate: \_ -> pure unit
  }

-- | Create a `Component` from a `Spec`, a `shouldComponentUpdate` function and an `Element`.
-- | This function subscribes the component to all state updates.
-- | Each update will trigger an equality check and possibly a rerender.
-- | You should avoid nesting those components because each of them has a separate subscription.
wiredL
  :: ∀ st stt act
   . Lens' st stt               -- | Lens for the part of state to use
  -> Spec st act                -- | Component spec
  -> (stt -> stt -> Boolean)    -- | Comparison function used to determine changes
  -> (stt -> Element st act)    -- | Element to be rendered
  -> Component st act
wiredL lens spec shouldUpdate el = React.component spec.displayName constructor
  where
    constructor this = do
      unsubscribeRef <- newSubscriptionRef
      initial <- React.getProps this
      let initialState = initial.state
      pure $
        { render: do
            {effect} <- React.getProps this
            {state} <- React.getState this
            pure $ el (view lens state) effect state
        , state: {state: initialState}
        , componentDidMount: do
            {effect} <- React.getProps this
            effect $ setupSubscription unsubscribeRef this *> spec.componentDidMount
        , shouldComponentUpdate: \_ {state} -> do
            current <- React.getState this
            pure $ shouldUpdate (view lens current.state) (view lens state)
        , componentDidUpdate: \{effect, state} _ _ ->
            effect $ spec.componentDidUpdate state
        , componentWillUnmount: do
            {effect} <- React.getProps this
            unsub <- Ref.read unsubscribeRef
            effect $ unsub *> spec.componentWillUnmount
        }
    newSubscriptionRef = Ref.new $ pure unit
    setupSubscription ref this = do
      subId <- subscribe $ \o n ->
        if shouldUpdate (view lens o) (view lens n)
        then void $ makeAff \cb -> do
          void $ React.writeStateWithCallback this {state: n} (cb $ Right n)
          pure nonCanceler
        else pure unit
      liftEffect $ Ref.write (unsubscribe subId) ref

-- | Create a `Component` from a `Spec` and an `Element`.
-- | This function subscribes the component to all state updates.
-- | It's the same as `wiredL`, but defaults to the identity lens.
wired
  :: ∀ st act
   . Spec st act               -- | Component spec
  -> (st -> st -> Boolean)     -- | Comparison function used to determine changes
  -> (st -> Element st act)    -- | Element to be rendered
  -> Component st act
wired spec shouldUpdate el = wiredL identity spec shouldUpdate el

-- | Create a `Component` from a `Spec` and an `Element`.
-- | This function subscribes the component to all state updates.
-- | It's the same as `wiredL`, but depends on an `Eq` instance to do equality checks.
wiredLEq
  :: ∀ st stt act
   . (Eq stt)
  => Lens' st stt               -- | Lens for the part of state to use  
  -> Spec st act                -- | Component spec
  -> (stt -> Element st act)     -- | Element to be rendered
  -> Component st act
wiredLEq lens spec el = wiredL lens spec (/=) el

-- | Create a `Component` from a `Spec` and an `Element`.
-- | This function subscribes the component to all state updates.
-- | It's the same as `wiredL`, but defaults to the identity lens and depends on an `Eq` instance to do equality checks.
wiredEq
  :: ∀ st act
   . (Eq st)
  => Spec st act                -- | Component spec
  -> (st -> Element st act)     -- | Element to be rendered
  -> Component st act
wiredEq spec el = wiredL identity spec (/=) el

-- | Shortcut for creating React stateless components that accept any state passed to it as props.
stateless :: ∀ st act. Element st act -> Component st act
stateless render = statelessComponent $ \{effect, state} -> render effect state

-- | Create a `Element` from a `Component`.
element :: ∀ st act. Component st act -> Element st act
element class_ effect state = React.createLeafElement class_ {effect, state}

-- | Embed a `Element` with a state of type `stt` into a `Element`
-- | with a state of type `st`.
zoom
  :: ∀ st stt act. ALens' st stt
  -> Element stt act
  -> Element st act
zoom lns cmp effect st = cmp (\e -> effect $ zoomEff (cloneLens lns) e) (st ^. cloneLens lns)

-- | Reify the current `Element` state.
state :: ∀ st act. (st -> Element st act) -> Element st act
state f effect st = (f st) effect st

-- | Reify `Element` substate specified by a lens.
stateL
  :: ∀ st stt act
   . ALens' st stt
  -> (stt -> Element st act)
  -> Element st act
stateL lns f effect st = (f (st ^. cloneLens lns)) effect st

-- | Modify `Element` substate specified by a `Lens`.
modifyL
  :: ∀ st stt act
   . ALens' st stt
  -> (stt -> stt)
  -> Eff st act Unit
modifyL lns f = modify (over (cloneLens lns) f)

lensAtA :: ∀ a. Int -> Lens' (Array a) a
lensAtA i = lens (\m -> unsafePartial $ fromJust $ index m i) (\m v -> fromMaybe m $ updateAt i v m)

lensAtM :: ∀ k v. Ord k => k -> Lens' (Map k v) v
lensAtM k = lens (\m -> unsafePartial $ fromJust $ Map.lookup k m) (\m v -> Map.insert k v m)

foreign import mapI :: ∀ a. Int -> (Int -> a) -> Array a

-- | Unfiltered `Array` traversal.
foreach
  :: ∀ st act a
   . ALens' st (Array a)
  -> (ALens' st a -> Element st act)
  -> Element st act
foreach = foreachF (const true)

-- | Filtered `Array` traversal.
foreachF
  :: ∀ st act a
   . (a -> Boolean)
  -> ALens' st (Array a)
  -> (ALens' st a -> Element st act)
  -> Element st act
foreachF f lns' cmp effect st =
  fragmentWithKey "" $ catMaybes $
    mapI (length st') \i -> case index st' i >>= pure <<< f of
      Just true  -> Just $ cmp (lns <<< lensAtA i) effect st
      Just false -> Nothing
      Nothing    -> Nothing
  where
    lns = cloneLens lns'
    st' = st ^. lns

-- | Zooming unfiltered `Array` traversal.
foreachZ
  :: ∀ st act a
   . ALens' st (Array a)
  -> Element a act
  -> Element st act
foreachZ = foreachZF (const true)

-- | Zooming filtered `Array` traversal.
foreachZF
  :: ∀ st act a
   . (a -> Boolean)
  -> ALens' st (Array a)
  -> Element a act
  -> Element st act
foreachZF f lns cmp = foreachF f lns \lns' -> zoom lns' cmp
