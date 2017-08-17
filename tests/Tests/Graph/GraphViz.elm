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
                        [ Node 0 { text = "Welcome" }
                        , Node 1 { text = "To" }
                        , Node 2 { text = "Web" }
                        , Node 3 { text = "GraphViz!" }
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
  rankdir=TB
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
                        [ Node 0 { text = "Welcome" }
                        , Node 1 { text = "To" }
                        , Node 2 { text = "Web" }
                        , Node 3 { text = "GraphViz!" }
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
  rankdir=LR
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
                            | rankdir = LR
                            , graph = "bgcolor=red"
                            , node = "shape=box, color=blue, style=\"rounded, filled\""
                        }

                    actual =
                        outputWithStyles myStyles g
                in
                    \() -> Expect.equal expected actual
            , test "with styles with overrides" <|
                let
                    n id text attrs =
                        Node id { text = text, attrs = attrs, other = "other" }

                    nodes =
                        [ n 0 "Welcome" ""
                        , n 1 "To" ""
                        , n 2 "Web" ""
                        , n 3 "GraphViz!" "style=\"bold,filled\""
                        ]

                    e from to attrs =
                        Edge from to { attrs = attrs, other = "other" }

                    edges =
                        [ e 0 1 ""
                        , e 1 2 ""
                        , e 1 3 "penwidth=5"
                        ]

                    myStyles =
                        { defaultStyles
                            | node = "style=rounded"
                        }

                    g =
                        Graph.fromNodesAndEdges nodes edges

                    expected =
                        """digraph G {
  rankdir=TB
  graph []
  node [style=rounded]
  edge []

  "Welcome" -> "To"
  "To" -> "Web"
  "To" -> "GraphViz!" [penwidth=5]

  "Welcome"
  "To"
  "Web"
  "GraphViz!" [style="bold,filled"]
}"""

                    actual =
                        outputWithStylesWithOverrides myStyles g
                in
                    \() -> Expect.equal expected actual
            ]
        ]
