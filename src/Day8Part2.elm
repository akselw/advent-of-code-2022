module Day8Part2 exposing (main)

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
        |> findScenicScores
        |> List.maximum
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


findTreesToTheLeft : Int -> Int -> TreeGrid -> List Int
findTreesToTheLeft rowIndex columnIndex treeGrid =
    treeGrid
        |> Dict.get rowIndex
        |> Maybe.map
            (\rowDict ->
                List.range 0 (columnIndex - 1)
                    |> List.reverse
                    |> List.filterMap (\i -> Dict.get i rowDict)
            )
        |> Maybe.withDefault []


findTreesToTheRight : Int -> Int -> TreeGrid -> List Int
findTreesToTheRight rowIndex columnIndex treeGrid =
    treeGrid
        |> Dict.get rowIndex
        |> Maybe.map
            (\rowDict ->
                List.range (columnIndex + 1) (Dict.size rowDict - 1)
                    |> List.filterMap (\i -> Dict.get i rowDict)
            )
        |> Maybe.withDefault []


findTreesAbove : Int -> Int -> TreeGrid -> List Int
findTreesAbove rowIndex columnIndex treeGrid =
    treeGrid
        |> Dict.filter (\key _ -> key < rowIndex)
        |> Dict.toList
        |> List.sortBy Tuple.first
        |> List.reverse
        |> List.map Tuple.second
        |> List.map (Dict.filter (\key _ -> key == columnIndex))
        |> List.concatMap Dict.values


findTreesBelow : Int -> Int -> TreeGrid -> List Int
findTreesBelow rowIndex columnIndex treeGrid =
    treeGrid
        |> Dict.filter (\key _ -> key > rowIndex)
        |> Dict.toList
        |> List.sortBy Tuple.first
        |> List.map Tuple.second
        |> List.map (Dict.filter (\key _ -> key == columnIndex))
        |> List.concatMap Dict.values


visibleFromLeft : Int -> Int -> Int -> TreeGrid -> Bool
visibleFromLeft rowIndex columnIndex height treeGrid =
    treeGrid
        |> findTreesToTheLeft rowIndex columnIndex
        |> List.all (\h -> h < height)


visibleFromRight : Int -> Int -> Int -> TreeGrid -> Bool
visibleFromRight rowIndex columnIndex height treeGrid =
    treeGrid
        |> findTreesToTheRight rowIndex columnIndex
        |> List.all (\h -> h < height)


visibleFromAbove : Int -> Int -> Int -> TreeGrid -> Bool
visibleFromAbove rowIndex columnIndex height treeGrid =
    treeGrid
        |> findTreesAbove rowIndex columnIndex
        |> List.all (\h -> h < height)


visibleFromBelow : Int -> Int -> Int -> TreeGrid -> Bool
visibleFromBelow rowIndex columnIndex height treeGrid =
    treeGrid
        |> findTreesBelow rowIndex columnIndex
        |> List.all (\h -> h < height)


numberOfVisibleTreesToTheLeft : Int -> Int -> Int -> TreeGrid -> Int
numberOfVisibleTreesToTheLeft rowIndex columnIndex height treeGrid =
    treeGrid
        |> findTreesToTheLeft rowIndex columnIndex
        |> numberOfVisibleTreesFromTree height


numberOfVisibleTreesToTheRight : Int -> Int -> Int -> TreeGrid -> Int
numberOfVisibleTreesToTheRight rowIndex columnIndex height treeGrid =
    treeGrid
        |> findTreesToTheRight rowIndex columnIndex
        |> numberOfVisibleTreesFromTree height


numberOfVisibleTreesAbove : Int -> Int -> Int -> TreeGrid -> Int
numberOfVisibleTreesAbove rowIndex columnIndex height treeGrid =
    treeGrid
        |> findTreesAbove rowIndex columnIndex
        |> numberOfVisibleTreesFromTree height


numberOfVisibleTreesBelow : Int -> Int -> Int -> TreeGrid -> Int
numberOfVisibleTreesBelow rowIndex columnIndex height treeGrid =
    treeGrid
        |> findTreesBelow rowIndex columnIndex
        |> numberOfVisibleTreesFromTree height


numberOfVisibleTreesFromTree : Int -> List Int -> Int
numberOfVisibleTreesFromTree height visibleHeights =
    if List.length visibleHeights == 0 then
        0

    else
        let
            smallerTrees =
                visibleHeights
                    |> List.Extra.takeWhile (\h -> h < height)
                    |> List.length
        in
        if smallerTrees == List.length visibleHeights then
            smallerTrees

        else
            smallerTrees + 1


findScenicScores : TreeGrid -> List Int
findScenicScores treeGrid =
    treeGrid
        |> Dict.map
            (\rowIndex row ->
                row
                    |> Dict.map
                        (\columnIndex height ->
                            numberOfVisibleTreesToTheLeft rowIndex columnIndex height treeGrid
                                * numberOfVisibleTreesAbove rowIndex columnIndex height treeGrid
                                * numberOfVisibleTreesToTheRight rowIndex columnIndex height treeGrid
                                * numberOfVisibleTreesBelow rowIndex columnIndex height treeGrid
                        )
            )
        |> Dict.values
        |> List.concatMap Dict.values


puzzleInput =
    String.trim """
30373
25512
65332
33549
35390
"""
