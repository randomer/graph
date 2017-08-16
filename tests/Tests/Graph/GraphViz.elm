module Tests.Graph.GraphViz exposing (all)

import Graph.GraphViz as Viz exposing (output)
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
  "Welcome" -> "To";
  "To" -> "Web";
  "To" -> "GraphViz!";

  "Welcome";
  "To";
  "Web";
  "GraphViz!";
}"""

                    actual =
                        output g
                in
                    \() -> Expect.equal expected actual
            ]
        ]
