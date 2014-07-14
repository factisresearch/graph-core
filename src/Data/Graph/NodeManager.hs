{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
module Data.Graph.NodeManager
    ( NodeManager (..), Node, NodeMap, NodeSet
    , emptyNode
    , initNodeManager, emptyNodeManager
    , getNodeHandle, getExistingNodeHandle, lookupNode, unsafeLookupNode
    , getNewNodesSince, isConsistent
    )
where

import Control.Monad.State.Strict
import Data.Hashable
import Data.Maybe
import Test.QuickCheck (NonNegative(..), Arbitrary(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.List as L

type Node = Int
type NodeMap v = IM.IntMap v
type NodeSet = IS.IntSet

emptyNode :: Node
emptyNode = -1

data NodeManager k
    = NodeManager
    { nm_idxToNode :: !(NodeMap k)
    , nm_nodeToIdx :: !(HM.HashMap k Node)
    , nm_nextIdx :: !Node
    } deriving (Show, Eq)

swap :: forall a b. (a, b) -> (b, a)
swap (x,y) = (y,x)

isConsistent :: (Ord k) => NodeManager k -> Bool
isConsistent (NodeManager{..}) =
    IM.size nm_idxToNode == HM.size nm_nodeToIdx
    && (IM.null nm_idxToNode || (nm_nextIdx > fst (IM.findMax nm_idxToNode)
                              && emptyNode < fst (IM.findMin nm_idxToNode)))
    && L.sort (HM.toList nm_nodeToIdx) == L.sort (map swap (IM.toList nm_idxToNode))

-- map must contain only non-negative keys!
initNodeManager :: (Hashable k, Eq k) => NodeMap k -> NodeManager k
initNodeManager nm =
    case IM.minViewWithKey nm of
       Just ((n, _), _) | n <= emptyNode -> error $ "Invalid node ID: " ++ show n
       _ -> NodeManager nm (invert nm) nextIdx
    where nextIdx
            | IM.null nm = 0
            | otherwise = 1 + fst (IM.findMax nm)
          invert im = HM.fromList . map swap $ IM.toList im

getNewNodesSince :: Node -> NodeManager k -> NodeMap k
getNewNodesSince n (NodeManager{..}) = snd $ IM.split n nm_idxToNode

emptyNodeManager :: forall k. NodeManager k
emptyNodeManager = NodeManager IM.empty HM.empty 0

getNodeHandle :: (Hashable k, Eq k, MonadState (NodeManager k) m) => k -> m Node
getNodeHandle k =
    do NodeManager{..} <- get
       case HM.lookup k nm_nodeToIdx of
          Just i -> return i
          Nothing ->
            do let i = nm_nextIdx
               put $ NodeManager { nm_idxToNode = IM.insert i k nm_idxToNode
                                 , nm_nodeToIdx = HM.insert k i nm_nodeToIdx
                                 , nm_nextIdx = i + 1
                                 }
               return i

getExistingNodeHandle :: (Hashable k, Eq k) => k -> NodeManager k -> Maybe Node
getExistingNodeHandle k (NodeManager{..}) = HM.lookup k nm_nodeToIdx

lookupNode :: Node -> NodeManager k -> Maybe k
lookupNode i (NodeManager{..}) = IM.lookup i nm_idxToNode

unsafeLookupNode :: Node -> NodeManager k -> k
unsafeLookupNode i nm = fromJust $ lookupNode i nm

instance Arbitrary v => Arbitrary (IM.IntMap v) where
    arbitrary = fmap (IM.fromList . map (\(NonNegative i, x) -> (i, x))) arbitrary
