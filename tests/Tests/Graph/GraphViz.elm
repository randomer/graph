module Tests.Graph.GraphViz exposing (all)

import Graph.GraphViz as Viz exposing (..)
import Graph exposing (Graph, NodeId, Node, Edge, NodeContext)
import Test exposing (..)
import Expect


all : Test
all =
    describe "GraphViz"
        [ describe "output" <|
            [ test "basic" <|
                let
                    nodes =
                        [ Node 0 "Welcome"
                        , Node 1 "To"
                        , Node 2 "Web"
                        , Node 3 "GraphViz!"
                        ]

                    e from to =
                        Edge from to ()

                    edges =
                        [ e 0 1
                        , e 1 2
                        , e 1 3
                        ]

                    g =
                        Graph.fromNodesAndEdges nodes edges

                    expected =
                        """digraph G {
  graph []
  node []
  edge []

  "Welcome" -> "To"
  "To" -> "Web"
  "To" -> "GraphViz!"

  "Welcome"
  "To"
  "Web"
  "GraphViz!"
}"""

                    actual =
                        output g
                in
                    \() -> Expect.equal expected actual
            , test "with styles" <|
                let
                    nodes =
                        [ Node 0 "Welcome"
                        , Node 1 "To"
                        , Node 2 "Web"
                        , Node 3 "GraphViz!"
                        ]

                    e from to =
                        Edge from to ()

                    edges =
                        [ e 0 1
                        , e 1 2
                        , e 1 3
                        ]

                    g =
                        Graph.fromNodesAndEdges nodes edges

                    expected =
                        """digraph G {
  graph [bgcolor=red]
  node [shape=box, color=blue, style="rounded, filled"]
  edge []

  "Welcome" -> "To"
  "To" -> "Web"
  "To" -> "GraphViz!"

  "Welcome"
  "To"
  "Web"
  "GraphViz!"
}"""

                    myStyles =
                        { defaultStyles
                            | graph = "bgcolor=red"
                            , node = "shape=box, color=blue, style=\"rounded, filled\""
                        }

                    actual =
                        outputWithStyles myStyles g
                in
                    \() -> Expect.equal expected actual
            ]
        ]
