module Day9Part1 exposing (main)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import List.Extra
import Set


main =
    puzzleInput
        |> String.lines
        |> List.filterMap parseInput
        |> performSteps
        |> Debug.toString
        |> text


type alias Command =
    { direction : Direction, number : Int }


type Direction
    = Up
    | Down
    | Left
    | Right


parseInput string =
    case String.words string of
        directionString :: numberString :: [] ->
            Maybe.map2 Command (parseDirection directionString) (String.toInt numberString)

        _ ->
            Nothing


parseDirection : String -> Maybe Direction
parseDirection string =
    case string of
        "R" ->
            Just Right

        "L" ->
            Just Left

        "U" ->
            Just Up

        "D" ->
            Just Down

        _ ->
            Nothing



--performSteps : List Command -> ??


type alias Coordinate =
    { x : Int
    , y : Int
    }


type alias State =
    { head : Coordinate
    , tail : Coordinate
    }


moveHead : Direction -> Coordinate -> Coordinate
moveHead direction coordinate =
    case direction of
        Up ->
            { coordinate | y = coordinate.y + 1 }

        Down ->
            { coordinate | y = coordinate.y - 1 }

        Left ->
            { coordinate | x = coordinate.x - 1 }

        Right ->
            { coordinate | x = coordinate.x + 1 }


coordinatesAreTouching : Coordinate -> Coordinate -> Bool
coordinatesAreTouching head tail =
    if head == tail then
        True

    else if head.x == tail.x && abs (head.y - tail.y) == 1 then
        True

    else if head.y == tail.y && abs (head.x - tail.x) == 1 then
        True

    else if abs (head.x - tail.x) == 1 && abs (head.y - tail.y) == 1 then
        True

    else
        False


moveTail : Coordinate -> Coordinate -> Coordinate
moveTail head tail =
    if coordinatesAreTouching head tail then
        tail

    else if head.x == tail.x then
        { x = tail.x
        , y =
            tail.y
                + (if head.y > tail.y then
                    1

                   else
                    -1
                  )
        }

    else
        { y = tail.y
        , x =
            tail.x
                + (if head.x > tail.x then
                    1

                   else
                    -1
                  )
        }


performSteps commands =
    List.foldl executeCommand
        { head = { x = 0, y = 0 }
        , tail = { x = 0, y = 0 }
        }
        commands


executeCommand : Command -> State -> State
executeCommand command initialState =
    let
        step : State -> State
        step state =
            let
                headLocation =
                    moveHead command.direction state.head
            in
            { head = headLocation
            , tail = moveTail headLocation state.tail
            }
    in
    List.range 1 command.number
        |> List.foldl (\_ b -> step b) initialState


puzzleInput =
    String.trim """
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
"""
