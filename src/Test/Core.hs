{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.Core where

import Data.Graph

import Control.Monad
import Test.Framework
import qualified Data.IntSet as IS

e :: (Node, Node) -> Edge
e (x,y) = Edge x y

testGraphA :: Graph
testGraphA = fromAdj [(1, [2,3]), (2, [3,4]), (3, [4])]

testGraphB :: Graph
testGraphB = fromEdges [e (1,2), e (2,1)]

test_hasEdge :: IO ()
test_hasEdge =
    do assertEqual True $ hasEdge 2 4 testGraphA
       forM_ (edges testGraphA) $ \(Edge a b) ->
          assertEqual True (hasEdge a b testGraphA)
       assertEqual False $ hasEdge 1 4 testGraphA
       assertEqual False $ hasEdge 1000 200 testGraphA

test_removeEdges :: IO ()
test_removeEdges =
    do assertEqual (fromAdj [(1, [2]), (2, [3,4]), (3, [4]), (4, [])])
                   (removeEdges [e (1,3), e (4,1)] testGraphA)

test_hull :: IO ()
test_hull =
    do assertEqual (IS.fromList [4]) (hull testGraphA 4)
       assertEqual (IS.fromList [1,2,3,4]) (hull testGraphA 1)
       assertEqual (IS.fromList [2,3,4]) (hull testGraphA 2)
       assertEqual (IS.fromList [3,4]) (hull testGraphA 3)
       assertEqual (IS.fromList [100]) (hull testGraphA 100)
       assertEqual (IS.fromList [1,2]) (hull testGraphB 1)

test_rhull :: IO ()
test_rhull =
    do assertEqual (IS.fromList [1,2,3]) (rhull testGraphA 3)
       assertEqual (IS.fromList [1]) (rhull testGraphA 1)
       assertEqual (IS.fromList [1,2]) (rhull testGraphB 1)

test_hullFold :: IO ()
test_hullFold =
    do assertEqual 10 (hullFold testGraphA (+) 0 1)
       assertEqual 4 (hullFold testGraphA (+) 0 4)
       assertEqual 100 (hullFold testGraphA (+) 0 100)
       assertEqual 3 (hullFold testGraphB (+) 0 1)

prop_fromEdgesAddEdges :: Graph -> Bool
prop_fromEdgesAddEdges g = isConsistent new && new == g
    where new = addEdges (edges g) empty

prop_fromEdgesToEdges :: Graph -> Bool
prop_fromEdgesToEdges g = isConsistent new && new == g
    where new = fromEdges (edges g)