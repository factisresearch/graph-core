{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Graph
  ( Graph, Node, NodeSet, Edge(..)
  , empty, fromEdges, fromAdj, isConsistent
  , nodes, edges, children, parents, hasEdge
  , edgeCount
  , hull, rhull, hullFold, hullFoldM, rhullFold
  , addEdge, addEdges, removeEdge, removeEdges
  , addNode, solitaireNodes
  , edgesAdj
  )
where

import Data.Graph.NodeManager hiding (isConsistent, nodes)
import Data.Graph.PureCore
