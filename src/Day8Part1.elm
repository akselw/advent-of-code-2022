module Day8Part1 exposing (main)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import List.Extra
import Set


main =
    puzzleInput
        |> toTreeGrid
        |> numberOfVisibleTrees
        |> Debug.toString
        |> text


type alias TreeGrid =
    Dict Int (Dict Int Int)


toTreeGrid : String -> TreeGrid
toTreeGrid string =
    string
        |> String.lines
        |> List.map lineToRow
        |> List.indexedMap Tuple.pair
        |> Dict.fromList


lineToRow : String -> Dict Int Int
lineToRow string =
    string
        |> String.split ""
        |> List.filterMap String.toInt
        |> List.indexedMap Tuple.pair
        |> Dict.fromList


numberOfVisibleTrees : TreeGrid -> Int
numberOfVisibleTrees treeGrid =
    treeGrid
        |> Dict.map
            (\rowIndex row ->
                row
                    |> Dict.map
                        (\columnIndex height ->
                            visibleFromLeft rowIndex columnIndex height treeGrid
                                || visibleFromAbove rowIndex columnIndex height treeGrid
                                || visibleFromRight rowIndex columnIndex height treeGrid
                                || visibleFromBelow rowIndex columnIndex height treeGrid
                        )
            )
        |> Dict.values
        |> List.concatMap Dict.values
        |> List.Extra.count identity


visibleFromLeft : Int -> Int -> Int -> TreeGrid -> Bool
visibleFromLeft rowIndex columnIndex height treeGrid =
    treeGrid
        |> Dict.get rowIndex
        |> Maybe.map
            (\rowDict ->
                List.range 0 (columnIndex - 1)
                    |> List.filterMap (\i -> Dict.get i rowDict)
                    |> List.all (\h -> h < height)
            )
        |> Maybe.withDefault True


visibleFromRight : Int -> Int -> Int -> TreeGrid -> Bool
visibleFromRight rowIndex columnIndex height treeGrid =
    treeGrid
        |> Dict.get rowIndex
        |> Maybe.map
            (\rowDict ->
                List.range (columnIndex + 1) (Dict.size rowDict - 1)
                    |> List.filterMap (\i -> Dict.get i rowDict)
                    |> List.all (\h -> h < height)
            )
        |> Maybe.withDefault True


visibleFromAbove : Int -> Int -> Int -> TreeGrid -> Bool
visibleFromAbove rowIndex columnIndex height treeGrid =
    treeGrid
        |> Dict.filter (\key _ -> key < rowIndex)
        |> Dict.map
            (\_ rowDict ->
                rowDict |> Dict.filter (\key _ -> key == columnIndex)
            )
        |> Dict.values
        |> List.concatMap Dict.values
        |> List.all (\h -> h < height)


visibleFromBelow : Int -> Int -> Int -> TreeGrid -> Bool
visibleFromBelow rowIndex columnIndex height treeGrid =
    treeGrid
        |> Dict.filter (\key _ -> key > rowIndex)
        |> Dict.map
            (\_ rowDict ->
                rowDict |> Dict.filter (\key _ -> key == columnIndex)
            )
        |> Dict.values
        |> List.concatMap Dict.values
        |> List.all (\h -> h < height)


puzzleInput =
    String.trim """
30373
25512
65332
33549
35390
"""
