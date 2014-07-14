{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.Persistence where

import Data.Graph
import Data.Graph.NodeManager
import Data.Graph.Persistence

import Test.Framework

prop_persistence :: NodeMap Char -> Graph -> Bool
prop_persistence nodeMap graph =
    let nodeMgr = initNodeManager nodeMap
        (nodeMgr', graph') = loadGraph (persistGraph nodeMgr graph)
    in (nodeMgr' == nodeMgr && graph == graph')
