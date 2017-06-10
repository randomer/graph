module Tests exposing (suite)


import Tests.Graph
import Tests.Graph.Tree

import Test exposing (..)


suite : Test
suite =
  describe "elm-graph"
    [ Tests.Graph.all
    , Tests.Graph.Tree.all
    ]
