module Tests exposing (all)


import Tests.Graph
import Tests.Graph.Tree

import Test exposing (..)


all : Test
all =
  describe "elm-graph"
    [ Tests.Graph.all
    , Tests.Graph.Tree.all
    ]
