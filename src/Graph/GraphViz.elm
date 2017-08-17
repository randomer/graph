module Graph.GraphViz
    exposing
        ( output
        , outputWithStyles
        , defaultStyles
        , Styles
        )

{-| This module provides a means of converting the `Graph` data type into a valid [GraphViz](http://www.graphviz.org/) string for visualizing your graph structure.

You can easily preview your graph by inserting the generated string into an online GraphViz tool like https://dreampuf.github.io/GraphvizOnline/.

You can also dynamically draw your graph in your application by sending the string over a port to the javascript version of the GraphViz library, https://github.com/mdaines/viz.js/ (see the examples there fore more specifics on how to embed the generated visualization).

@docs output

# Attrs

GraphViz allows for customizing the graph's look via "Attrs."

@docs Styles, defaultStyles, outputWithStyles
-}

import Graph exposing (Graph, Edge, Node, edges, nodes, get)


{-| Converts a `Graph` into a valid GraphViz string.  Note that the you must supply a `Graph String e` type, where the `String` is the label that should be printed in each node.
-}
output : Graph String e -> String
output =
    outputWithStyles defaultStyles


{-| A type representing the attrs to apply at the graph, node, and edge entities (subgraphs and cluster subgraphs are not supported).

Note that `Styles` is made up of strings, which loses type safety, but allows you to use any GraphViz attrs without having to model them out in entirety in this module.  It is up to you to make sure you provide valid attr strings.  See http://www.graphviz.org/content/attrs for available options.
-}
type alias Styles =
    { graph : String
    , node : String
    , edge : String
    }


{-| A blank `Styles` record to build from to define your own styles.

    myStyles =
        { defaultStyles
            | node = "shape=box, color=blue, style=\"rounded, filled\""
        }
-}
defaultStyles : Styles
defaultStyles =
    Styles "" "" ""


{-| Same as `output`, but allows you to add attrs to the graph.  These attrs will be applied to the entire graph.
-}
outputWithStyles : Styles -> Graph String e -> String
outputWithStyles styles graph =
    let
        getText id =
            get id graph
                |> Maybe.map (.node >> .label)
                |> Maybe.withDefault ("*Node id " ++ toString id ++ " not found*")

        edges =
            let
                sortEdges a b =
                    case compare a.from b.from of
                        LT ->
                            LT

                        GT ->
                            GT

                        EQ ->
                            compare a.to b.to
            in
                Graph.edges graph
                    |> List.sortWith sortEdges

        nodes =
            Graph.nodes graph

        edgesString =
            List.map edge edges
                |> String.join "\n"

        edge ({ from, to, label } as edge) =
            "  "
                ++ Basics.toString (getText from)
                ++ " -> "
                ++ Basics.toString (getText to)

        nodesString =
            List.map node nodes
                |> String.join "\n"

        node node =
            "  " ++ Basics.toString node.label

        graphStyles =
            "  graph [" ++ styles.graph ++ "]"

        nodeStyles =
            "  node [" ++ styles.node ++ "]"

        edgeStyles =
            "  edge [" ++ styles.edge ++ "]"
    in
        "digraph G {\n"
            ++ graphStyles
            ++ "\n"
            ++ nodeStyles
            ++ "\n"
            ++ edgeStyles
            ++ "\n\n"
            ++ edgesString
            ++ "\n\n"
            ++ nodesString
            ++ "\n}"
