{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import {-@ HTF_TESTS @-} Test.NodeManager
import {-@ HTF_TESTS @-} Test.Core

main :: IO ()
main = htfMain htf_importedTests
