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
        |> countTailPositions
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


type alias StateAndHistory =
    { state : State
    , history : List Coordinate
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


moveTail : Direction -> Coordinate -> Coordinate -> Coordinate
moveTail headDirection head tail =
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

    else if head.y == tail.y then
        { y = tail.y
        , x =
            tail.x
                + (if head.x > tail.x then
                    1

                   else
                    -1
                  )
        }

    else
        moveAdjescentToHead headDirection head tail


moveAdjescentToHead : Direction -> Coordinate -> Coordinate -> Coordinate
moveAdjescentToHead headDirection head tail =
    case headDirection of
        Up ->
            { x = head.x
            , y = head.y - 1
            }

        Down ->
            { x = head.x
            , y = head.y + 1
            }

        Right ->
            { y = head.y
            , x = head.x - 1
            }

        Left ->
            { y = head.y
            , x = head.x + 1
            }


performSteps : List Command -> StateAndHistory
performSteps commands =
    List.foldl executeCommand
        { state =
            { head = { x = 0, y = 0 }
            , tail = { x = 0, y = 0 }
            }
        , history = [ { x = 0, y = 0 } ]
        }
        commands


executeCommand : Command -> StateAndHistory -> StateAndHistory
executeCommand command initial =
    let
        step : StateAndHistory -> StateAndHistory
        step { state, history } =
            let
                headLocation : Coordinate
                headLocation =
                    moveHead command.direction state.head

                tailLocation : Coordinate
                tailLocation =
                    moveTail command.direction headLocation state.tail
            in
            { state =
                { head = headLocation
                , tail = tailLocation
                }
            , history = tailLocation :: history
            }
    in
    List.range 1 command.number
        |> List.foldl (\_ b -> step b) { state = initial.state, history = initial.history }



--countTailPositions : StateAndHistory -> Int


countTailPositions { history } =
    history
        |> List.map (\coordinate -> ( coordinate.x, coordinate.y ))
        |> Set.fromList
        |> Set.size


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
