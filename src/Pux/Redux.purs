module Pux.Redux where

import Control.Monad.Eff (Eff, kind Effect)
import Data.Symbol (class IsSymbol, SProxy(..))
import Prelude (Unit, map, ($))
import Pux (CoreEffects, FoldP, noEffects)

-- | Effect for actions that modify a redux store
foreign import data REDUX :: Effect 

-- | A redux action is a record containing at least a "type" field
type Action r = { type :: String | r }

-- | The type of redux's dispatch function
type Dispatch r fx = Action r -> Eff (CoreEffects (redux :: REDUX | fx)) Unit

-- | A record containing a dispatch function along with some other
-- fields. This will be used for the state of the pux application
type Props payload props fx = { dispatch :: Dispatch payload fx | props }

data Event pl fx ev
  = SetDispatch (Dispatch pl fx)
  | AppEvent ev

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

mkFoldp
  :: forall props ev fx pl
   . (Dispatch pl fx -> FoldP (Record props) ev (redux :: REDUX | fx))
  -> FoldP (Props pl props fx) (Event pl fx ev) (redux :: REDUX | fx)
mkFoldp foldpf (SetDispatch f) st = noEffects $ st { dispatch = f } 
mkFoldp foldpf (AppEvent ev) st =
  let
    { effects, state } = foldpf st.dispatch ev (removeField (SProxy :: SProxy "dispatch") st)
  in 
    { effects: map (map (map AppEvent)) effects
    , state: merge { dispatch: st.dispatch } state
    }
