module Pux.Redux where

import Control.Monad.Eff (Eff, kind Effect)
import Data.Symbol (class IsSymbol, SProxy(..))
import Prelude (Unit, const, map, pure, unit, ($), (<<<))
import Pux (CoreEffects, FoldP, Config, noEffects)
import Pux.DOM.Events (DOMEvent)
import Pux.DOM.HTML (HTML)
import Signal (Signal)

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

type AppConfig props pl fx ev =
  { initialState :: Record props
  , view :: Record props -> HTML (AppEvent pl fx ev)
  , foldp :: Dispatch pl fx -> FoldP (Record props) ev (redux :: REDUX | fx)
  , inputs :: Array (Signal (AppEvent pl fx ev))
  }

fromAppConfig
  :: forall pl fx ev props
   . AppConfig props pl fx ev
  -> Config
      (DOMEvent -> AppEvent pl fx ev)
      (AppEvent pl fx ev)
      (AppState pl props fx)
      (redux :: REDUX | fx)
fromAppConfig { initialState, view, foldp, inputs } =
  { initialState: mkInitialState initialState
  , view: mkView view
  , foldp: mkFoldp foldp
  , inputs
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
