module Day12Part1 exposing (main)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, p, text)
import Html.Events exposing (onClick)
import List.Extra
import Set exposing (Set)


main =
    puzzleInput
        |> parseGrid
        |> toGridInfo
        |> Maybe.map findPathsToEndPoint
        |> Maybe.withDefault Set.empty
        |> Set.toList
        --|> List.length
        |> List.sortBy List.length
        |> List.head
        |> Maybe.map List.reverse
        |> Maybe.map List.length
        |> Debug.toString
        |> text


type alias Coordinate =
    ( Int, Int )


type alias Grid =
    Dict Coordinate Char


type alias GridInfo =
    { grid : Grid
    , startingPoint : Coordinate
    , endPoint : Coordinate
    }


parseGrid : String -> Grid
parseGrid string =
    string
        |> String.lines
        |> List.map String.toList
        |> List.indexedMap (\rowIndex charList -> List.indexedMap (\columnIndex char -> ( ( columnIndex, rowIndex ), char )) charList)
        |> List.concat
        |> Dict.fromList


toGridInfo : Grid -> Maybe GridInfo
toGridInfo grid =
    Maybe.map2 (GridInfo grid)
        (findStartingPoint grid)
        (findEndPoint grid)


findStartingPoint : Grid -> Maybe Coordinate
findStartingPoint grid =
    grid
        |> Dict.toList
        |> List.Extra.find (\( _, char ) -> char == 'S')
        |> Maybe.map Tuple.first


findEndPoint : Grid -> Maybe Coordinate
findEndPoint grid =
    grid
        |> Dict.toList
        |> List.Extra.find (\( _, char ) -> char == 'E')
        |> Maybe.map Tuple.first



--findPathsToEndPoint : GridInfo -> a


findPathsToEndPoint gridInfo =
    findAllPaths gridInfo [] gridInfo.startingPoint 'a'


findAllPaths : GridInfo -> List Coordinate -> Coordinate -> Char -> Set (List Coordinate)
findAllPaths gridInfo currentPath currentCoords currentHeight =
    let
        newPath : List Coordinate
        newPath =
            currentCoords :: currentPath
    in
    if isEndpoint currentHeight then
        Set.singleton newPath

    else
        [ coordsToTheRight currentCoords
        , coordsBelow currentCoords
        , coordsToTheLeft currentCoords
        , coordsAbove currentCoords
        ]
            |> List.map
                (findAllPathsNotVisited
                    gridInfo
                    newPath
                    currentHeight
                )
            |> mergeSets


isEndpoint : Char -> Bool
isEndpoint char =
    char == 'E'


coordsToTheRight : Coordinate -> Coordinate
coordsToTheRight ( x, y ) =
    ( x + 1, y )


coordsToTheLeft : Coordinate -> Coordinate
coordsToTheLeft ( x, y ) =
    ( x - 1, y )


coordsAbove : Coordinate -> Coordinate
coordsAbove ( x, y ) =
    ( x, y - 1 )


coordsBelow : Coordinate -> Coordinate
coordsBelow ( x, y ) =
    ( x, y + 1 )


findAllPathsNotVisited : GridInfo -> List Coordinate -> Char -> Coordinate -> Set (List Coordinate)
findAllPathsNotVisited gridInfo currentPath currentHeight nextCoordinate =
    if not (List.member nextCoordinate currentPath) then
        case Dict.get nextCoordinate gridInfo.grid of
            Just 'E' ->
                if currentHeight == 'z' || currentHeight == 'y' then
                    findAllPaths gridInfo currentPath nextCoordinate 'E'

                else
                    Set.empty

            Just nextHeight ->
                if Char.toCode nextHeight <= (Char.toCode currentHeight + 1) then
                    findAllPaths gridInfo currentPath nextCoordinate nextHeight

                else
                    Set.empty

            Nothing ->
                Set.empty

    else
        Set.empty


mergeSets : List (Set comparable) -> Set comparable
mergeSets sets =
    List.foldl Set.union Set.empty sets


puzzleInput =
    String.trim """
Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
"""
