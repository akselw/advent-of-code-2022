module Day12Part1 exposing (main)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, p, text)
import Html.Events exposing (onClick)
import List.Extra
import Maybe.Extra
import Set exposing (Set)


main =
    puzzleInput
        |> parseGrid
        |> toGridInfo
        |> Maybe.map calculateDistances
        |> Maybe.andThen distanceOfEndpoint
        |> Debug.toString
        |> text


type alias Coordinate =
    ( Int, Int )


type alias Grid =
    Dict Coordinate GridPoint


type GridPointStatus
    = NotVisited
    | Unfinished Int
    | Visited Int


type alias GridPoint =
    { char : Char
    , neighbours : List Coordinate
    , status : GridPointStatus
    }


type alias GridInfo =
    { grid : Grid
    , startingPoint : Coordinate
    , endPoint : Coordinate
    }


parseGrid : String -> Dict Coordinate Char
parseGrid string =
    string
        |> String.lines
        |> List.map String.toList
        |> List.indexedMap (\rowIndex charList -> List.indexedMap (\columnIndex char -> ( ( columnIndex, rowIndex ), char )) charList)
        |> List.concat
        |> Dict.fromList


toGridInfo : Dict Coordinate Char -> Maybe GridInfo
toGridInfo grid =
    Maybe.map2 (GridInfo (coordsToGrid grid))
        (findStartingPoint grid)
        (findEndPoint grid)


coordsToGrid : Dict Coordinate Char -> Grid
coordsToGrid dict =
    let
        charToGridPoint coords char =
            { char = char
            , status = NotVisited
            , neighbours = findNeighbours dict coords char
            }
    in
    Dict.map charToGridPoint dict


findNeighbours : Dict Coordinate Char -> Coordinate -> Char -> List Coordinate
findNeighbours dict coords char =
    [ coordsToTheRight coords
        |> ifReachable dict char
    , coordsBelow coords
        |> ifReachable dict char
    , coordsToTheLeft coords
        |> ifReachable dict char
    , coordsAbove coords
        |> ifReachable dict char
    ]
        |> Maybe.Extra.values


charToHeight : Char -> Int
charToHeight char =
    if char == 'S' then
        Char.toCode 'a'

    else if char == 'E' then
        Char.toCode 'z'

    else
        Char.toCode char


ifReachable : Dict Coordinate Char -> Char -> Coordinate -> Maybe Coordinate
ifReachable dict originChar neighbourCoords =
    case Dict.get neighbourCoords dict of
        Just neighbourChar ->
            if charToHeight neighbourChar <= (charToHeight originChar + 1) then
                Just neighbourCoords

            else
                Nothing

        Nothing ->
            Nothing


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


findStartingPoint : Dict Coordinate Char -> Maybe Coordinate
findStartingPoint grid =
    grid
        |> Dict.toList
        |> List.Extra.find (\( _, char ) -> char == 'S')
        |> Maybe.map Tuple.first


findEndPoint : Dict Coordinate Char -> Maybe Coordinate
findEndPoint grid =
    grid
        |> Dict.toList
        |> List.Extra.find (\( _, char ) -> char == 'E')
        |> Maybe.map Tuple.first


calculateDistances : GridInfo -> GridInfo
calculateDistances gridInfo =
    let
        startingGrid : Grid
        startingGrid =
            Dict.update gridInfo.startingPoint (Maybe.map (setValue 0)) gridInfo.grid
    in
    { gridInfo | grid = dijkstrasAlgorithm startingGrid }


dijkstrasAlgorithm : Grid -> Grid
dijkstrasAlgorithm grid =
    let
        maybeCurrentPoint : Maybe ( Coordinate, GridPoint )
        maybeCurrentPoint =
            findNextActivePoint grid
                |> Debug.log "maybeCurrentPoint"
    in
    case maybeCurrentPoint of
        Just ( coords, currentPoint ) ->
            case currentPoint.status of
                Unfinished value ->
                    let
                        updateValue : Coordinate -> Grid -> Grid
                        updateValue neighbourCoords dict =
                            Dict.update neighbourCoords (Maybe.map (setValue (value + 1))) dict

                        updatedGrid =
                            List.foldl updateValue grid currentPoint.neighbours
                                |> Dict.update coords (Maybe.map setFinished)
                    in
                    dijkstrasAlgorithm updatedGrid

                _ ->
                    grid

        Nothing ->
            grid


findNextActivePoint : Grid -> Maybe ( Coordinate, GridPoint )
findNextActivePoint grid =
    let
        minUnfinishedValue : Coordinate -> GridPoint -> Maybe ( Coordinate, GridPoint ) -> Maybe ( Coordinate, GridPoint )
        minUnfinishedValue coords gridPoint maybeLowestSoFar =
            let
                lowestStatus : GridPointStatus
                lowestStatus =
                    maybeLowestSoFar
                        |> Maybe.map Tuple.second
                        |> Maybe.map .status
                        |> Maybe.withDefault NotVisited
            in
            case ( lowestStatus, gridPoint.status ) of
                ( Unfinished soFarValue, Unfinished thisValue ) ->
                    if thisValue < soFarValue then
                        Just ( coords, gridPoint )

                    else
                        maybeLowestSoFar

                ( NotVisited, Unfinished v ) ->
                    Just ( coords, gridPoint )

                _ ->
                    maybeLowestSoFar
    in
    Dict.foldl minUnfinishedValue Nothing grid


setValue : Int -> GridPoint -> GridPoint
setValue value gridPoint =
    { gridPoint
        | status =
            case gridPoint.status of
                Unfinished previousValue ->
                    Unfinished (min previousValue value)

                NotVisited ->
                    Unfinished value

                Visited _ ->
                    gridPoint.status
    }


setFinished : GridPoint -> GridPoint
setFinished gridPoint =
    { gridPoint
        | status =
            case gridPoint.status of
                Unfinished value ->
                    Visited value

                NotVisited ->
                    NotVisited

                Visited _ ->
                    gridPoint.status
    }


distanceOfEndpoint : GridInfo -> Maybe GridPointStatus
distanceOfEndpoint gridInfo =
    gridInfo.grid
        |> Dict.get gridInfo.endPoint
        |> Maybe.map .status


puzzleInput =
    String.trim """
Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
"""
