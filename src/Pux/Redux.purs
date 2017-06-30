module Pux.Redux
  ( REDUX
  , Action
  , Reducer
  , Dispatch
  , initialDispatch
  , AppState
  , AppEvent
  , appEvent
  , AppConfig
  , fromAppConfig
  ) where

import Prelude 

import Control.Monad.Eff (Eff, kind Effect)
import Data.Array ((:))
import Data.Symbol (class IsSymbol, SProxy(..))
import Pux (CoreEffects, FoldP, Config, noEffects)
import Pux.DOM.Events (DOMEvent)
import Pux.DOM.HTML (HTML)
import Signal (Signal)
import Text.Smolder.Markup (EventHandlers)

-- | Effect for actions that modify a redux store
foreign import data REDUX :: Effect 

-- | The type of redux actions.
-- |
-- | An action is a record containing at least a "type" field
-- |
-- | ```purescript
-- | increment :: Int -> Action (value :: Int)
-- | increment value = { type: "INCREMENT", value }
-- | ```
type Action r = { type :: String | r }

-- | The type of a redux reducer
-- |
-- | A reducer is a function that takes a state and an action, and returns a new
-- | state based on that action.
-- |
-- | ```purescript
-- | reducer :: Reducer Int (value :: Int)
-- | reducer state action =
-- |   case action.type of
-- |     "INCREMENT" -> state + action.value
-- |     _ -> state
-- | ```
type Reducer s r = s -> Action r -> s

-- | The type of redux's dispatch function
-- |
-- | A dispatch function takes an action as an argument and returns an effectul
-- | computation that modifies the redux store, and has some other unknown effects.
-- |
-- | ```purescript
-- | foldp
-- |   :: forall fx ev st
-- |    . Dispatch (value :: Int) fx
-- |   -> FoldP st ev (redux :: REDUX | fx)
-- | foldp dispatch ev st =
-- |   case ev of
-- |     ... ->
-- |       { state: ...
-- |       , effects:
-- |         [ ...
-- |         , liftEff (dispatch $ increment 5) $> Nothing
-- |         ]
-- |       }
-- | ```
type Dispatch r fx = Action r -> Eff (CoreEffects (redux :: REDUX | fx)) Unit

-- | A safe initial value for the dispatch function. This will normally be used
-- | from Javascript.
initialDispatch :: forall r fx. Dispatch r fx
initialDispatch = const $ pure unit

-- | The state type for apps that have access to a dispatch function.
-- |
-- | You will never have to create a value of this type manually.
type AppState payload st fx = { dispatch :: Dispatch payload fx | st }

-- | The event type for apps that have access to a dispatch function
-- |
-- | You will never have to create a value of this type manually.
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
-- |
-- | ```purescript
-- | textInput :: forall ev pl fx. (String -> ev) -> HTML (AppEvent pl fx ev)
-- | textInput event =
-- |   ! type' "text"
-- |   #! appEvent onChange (event <<< targetValue)
-- | ```
appEvent
  :: (forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev))
  -> forall event pl fx
   . (DOMEvent -> event)
  -> EventHandlers (DOMEvent -> AppEvent pl fx event)
appEvent handlerType handler = handlerType (AppEvent <<< handler)

-- | The configuration of a pux app that can interact with a redux store.
-- |
-- | Use 'fromAppConfig' to obtain a regular 'Config'
type AppConfig props pl fx ev =
  { initialState :: Record props
  , view :: Record props -> HTML (AppEvent pl fx ev)
  , foldp :: Dispatch pl fx -> FoldP (Record props) ev (redux :: REDUX | fx)
  , dispatch :: Signal (Dispatch pl fx)
  , inputs :: Array (Signal ev)
  }

-- | Convert an 'AppConfig' to a 'Config'
-- |
-- | ```purescript
-- | main = do
-- |   app <- start $ fromAppConfig
-- |     { initialState
-- |     , foldp
-- |     , view
-- |     , dispatch
-- |     , inputs: []
-- |     }
-- |   ...
-- | ```
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

-- **Unsafe**
-- 
-- Record restriction
--
-- Because there is no `Lacks` constraint, the result of `restrict`
-- could still contain the label that was deleted and therefore allow
-- access to a field that doesn't exist.
foreign import unsafeRestrict
  :: forall label a tail result
   . IsSymbol label
  => RowCons label a tail result
  => SProxy label
  -> Record result
  -> Record tail

-- **Unsafe**
--
-- Record extension
--
-- See `unsafeRestrict` for details
foreign import unsafeExtend
  :: forall label a tail result
   . IsSymbol label
  => RowCons label a tail result
  => SProxy label
  -> a
  -> Record tail
  -> Record result

mkInitialState
  :: forall props pl fx
   . Record props
  -> AppState pl props fx
mkInitialState = unsafeExtend (SProxy :: SProxy "dispatch") initialDispatch

mkFoldp
  :: forall props ev fx pl
   . (Dispatch pl fx -> FoldP (Record props) ev (redux :: REDUX | fx))
  -> FoldP (AppState pl props fx) (AppEvent pl fx ev) (redux :: REDUX | fx)
mkFoldp foldpf (SetDispatch f) st = noEffects $ st { dispatch = f } 
mkFoldp foldpf (AppEvent ev) st =
  let
    { effects, state } = foldpf st.dispatch ev (unsafeRestrict (SProxy :: SProxy "dispatch") st)
  in 
    { effects: map (map (map AppEvent)) effects
    , state: unsafeExtend (SProxy :: SProxy "dispatch") st.dispatch state
    }

mkView
  :: forall props pl fx ev
   . (Record props -> HTML (AppEvent pl fx ev))
  -> AppState pl props fx -> HTML (AppEvent pl fx ev)
mkView f = f <<< unsafeRestrict (SProxy :: SProxy "dispatch")
