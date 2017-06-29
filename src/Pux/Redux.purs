module Pux.Redux where

import Control.Monad.Eff (Eff, kind Effect)
import Data.Array ((:))
import Data.Symbol (class IsSymbol, SProxy(..))
import Prelude (class Applicative, class Apply, class Bind, class Functor, Unit, const, map, pure, unit, ($), (<<<))
import Pux (CoreEffects, FoldP, Config, noEffects)
import Pux.DOM.Events (DOMEvent)
import Pux.DOM.HTML (HTML)
import Signal (Signal)
import Text.Smolder.Markup (EventHandlers)

-- | Effect for actions that modify a redux store
foreign import data REDUX :: Effect 

-- | A redux action is a record containing at least a "type" field
type Action r = { type :: String | r }

-- | The type of a redux reducer
type Reducer s r = s -> Action r -> s

-- | The type of redux's dispatch function
type Dispatch r fx = Action r -> Eff (CoreEffects (redux :: REDUX | fx)) Unit

-- | A safe initial value for the dispatch function
initialDispatch :: forall r fx. Dispatch r fx
initialDispatch = const $ pure unit

-- | A record containing a dispatch function along with some other
-- fields. This will be used for the state of the pux application
type AppState payload props fx = { dispatch :: Dispatch payload fx | props }

data AppEvent pl fx ev
  = SetDispatch (Dispatch pl fx)
  | AppEvent ev

instance appEventFunctor :: Functor (AppEvent pl fx) where
  map f (AppEvent ev) = AppEvent (f ev)
  map f (SetDispatch dis) = SetDispatch dis

instance appEventApply :: Apply (AppEvent pl fx) where
  apply (AppEvent f) (AppEvent a) = AppEvent (f a)
  apply (SetDispatch dis) _ = SetDispatch dis
  apply _ (SetDispatch dis) = SetDispatch dis

instance appEventApplicative :: Applicative (AppEvent pl fx) where
  pure = AppEvent

instance appEventBind :: Bind (AppEvent pl fx) where
  bind (AppEvent ev) f = f ev
  bind (SetDispatch dis) f = SetDispatch dis

-- | Lift an event source from supplying `event`s to supplying `AppEvent pl fx event`s
--
-- Example:
--
-- input ! type' "text" #! appEvent onChange (TextChangedEvent <<< targetValue)
appEvent
  :: (forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev))
  -> forall event pl fx
   . (DOMEvent -> event)
  -> EventHandlers (DOMEvent -> AppEvent pl fx event)
appEvent handlerType handler = handlerType (AppEvent <<< handler)

type AppConfig props pl fx ev =
  { initialState :: Record props
  , view :: Record props -> HTML (AppEvent pl fx ev)
  , foldp :: Dispatch pl fx -> FoldP (Record props) ev (redux :: REDUX | fx)
  , dispatch :: Signal (Dispatch pl fx)
  , inputs :: Array (Signal ev)
  }

fromAppConfig
  :: forall pl fx ev props
   . AppConfig props pl fx ev
  -> Config
      (DOMEvent -> AppEvent pl fx ev)
      (AppEvent pl fx ev)
      (AppState pl props fx)
      (redux :: REDUX | fx)
fromAppConfig { initialState, view, foldp, dispatch, inputs } =
  { initialState: mkInitialState initialState
  , view: mkView view
  , foldp: mkFoldp foldp
  , inputs: map SetDispatch dispatch : map (map AppEvent) inputs
  }

-- | Right-biased record merge
foreign import merge
  :: forall r1 r2 r3
   . Union r1 r2 r3
  => Record r1
  -> Record r2
  -> Record r3

foreign import removeField
  :: forall l a r r'
   . IsSymbol l
  => RowCons l a r r'
  => SProxy l
  -> Record r'
  -> Record r

foreign import addField
  :: forall l a r r'
   . IsSymbol l
  => RowCons l a r r'
  => SProxy l
  -> a
  -> Record r
  -> Record r'

mkInitialState
  :: forall props pl fx
   . Record props
  -> AppState pl props fx
mkInitialState = addField (SProxy :: SProxy "dispatch") initialDispatch

mkFoldp
  :: forall props ev fx pl
   . (Dispatch pl fx -> FoldP (Record props) ev (redux :: REDUX | fx))
  -> FoldP (AppState pl props fx) (AppEvent pl fx ev) (redux :: REDUX | fx)
mkFoldp foldpf (SetDispatch f) st = noEffects $ st { dispatch = f } 
mkFoldp foldpf (AppEvent ev) st =
  let
    { effects, state } = foldpf st.dispatch ev (removeField (SProxy :: SProxy "dispatch") st)
  in 
    { effects: map (map (map AppEvent)) effects
    , state: merge { dispatch: st.dispatch } state
    }

mkView
  :: forall props pl fx ev
   . (Record props -> HTML (AppEvent pl fx ev))
  -> AppState pl props fx -> HTML (AppEvent pl fx ev)
mkView f = f <<< removeField (SProxy :: SProxy "dispatch")
