module Graph.GraphViz exposing (output)

{-| This module provides a means of converting the `Graph` data type into a valid [GraphViz](http://www.graphviz.org/) string for visualizing your graph structure.

 You can easily preview your graph by inserting the generated string into an online GraphViz tool like https://dreampuf.github.io/GraphvizOnline/.

 You can also dynamically draw your graph in your application by sending the string over a port to the javascript version of the GraphViz library, https://github.com/mdaines/viz.js/ (see the examples there fore more specifics on how to embed the generated visualization).

Note that this currently only supports a subset of the full GraphViz language.

@docs output
-}

import Graph exposing (Graph, Edge, Node, edges, nodes, get)


{-| Converts a `Graph` into a valid GraphViz string.  Note that the you must supply a `Graph String e` type, where the `String` is the label that should be printed in each node.
-}
output : Graph String e -> String
output graph =
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
                |> String.join ";\n"

        edge ({ from, to, label } as edge) =
            "  "
                ++ Basics.toString (getText from)
                ++ " -> "
                ++ Basics.toString (getText to)

        nodesString =
            List.map node nodes
                |> String.join ";\n"

        node node =
            "  " ++ Basics.toString node.label
    in
        "digraph G {\n" ++ edgesString ++ ";\n\n" ++ nodesString ++ ";\n}"
