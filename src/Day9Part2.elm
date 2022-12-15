module Day9Part2 exposing (main)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, p, text)
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



--|> .history
--|> List.reverse
--|> Debug.toString
--|> String.split "},{"
--|> List.map (text >> List.singleton >> p [])
--|> div []


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


type alias Coordinate =
    { x : Int
    , y : Int
    }


type alias State =
    { head : Coordinate
    , firstTail : Coordinate
    , tail : List Coordinate
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


findTailMoveDirection : Direction -> Coordinate -> Coordinate -> TailMoveDirection
findTailMoveDirection headDirection head tail =
    if coordinatesAreTouching head tail then
        NoMovement

    else if head.x == tail.x then
        if head.y > tail.y then
            North

        else
            South

    else if head.y == tail.y then
        if head.x > tail.x then
            East

        else
            West

    else
        moveNonAdjescentToHead headDirection head tail


moveNonAdjescentToHead : Direction -> Coordinate -> Coordinate -> TailMoveDirection
moveNonAdjescentToHead headDirection head oldTail =
    case headDirection of
        Up ->
            if head.x > oldTail.x then
                NorthEast

            else
                NorthWest

        Down ->
            if head.x > oldTail.x then
                SouthEast

            else
                SouthWest

        Right ->
            if head.y > oldTail.y then
                NorthEast

            else
                SouthEast

        Left ->
            if head.y > oldTail.y then
                NorthWest

            else
                SouthWest


updateTail : TailMoveDirection -> Coordinate -> List Coordinate -> List Coordinate
updateTail previousTailMoveDirection theTailBeforeThisOne tail =
    case tail of
        [] ->
            []

        currentTail :: rest ->
            let
                currentTailMoveDirection : TailMoveDirection
                currentTailMoveDirection =
                    findNextTailMoveDirection previousTailMoveDirection theTailBeforeThisOne currentTail

                a : Coordinate
                a =
                    tailMoveDirectionToCoordinates currentTailMoveDirection currentTail
            in
            a :: updateTail currentTailMoveDirection a rest


findNextTailMoveDirection : TailMoveDirection -> Coordinate -> Coordinate -> TailMoveDirection
findNextTailMoveDirection previousTailMoveDirection theTailBeforeThisOne currentTail =
    if coordinatesAreTouching theTailBeforeThisOne currentTail then
        NoMovement

    else if theTailBeforeThisOne.x == currentTail.x then
        if theTailBeforeThisOne.y > currentTail.y then
            North

        else
            South

    else if theTailBeforeThisOne.y == currentTail.y then
        if theTailBeforeThisOne.x > currentTail.x then
            East

        else
            West

    else
        moveNonAdjescentToTail previousTailMoveDirection theTailBeforeThisOne currentTail


moveNonAdjescentToTail : TailMoveDirection -> Coordinate -> Coordinate -> TailMoveDirection
moveNonAdjescentToTail previousTailMoveDirection theTailBeforeThisOne currentTail =
    case previousTailMoveDirection of
        North ->
            if theTailBeforeThisOne.x > currentTail.x then
                NorthEast

            else
                NorthWest

        NorthEast ->
            NorthEast

        East ->
            if theTailBeforeThisOne.y > currentTail.y then
                NorthEast

            else
                SouthEast

        SouthEast ->
            SouthEast

        South ->
            if theTailBeforeThisOne.x > currentTail.x then
                SouthEast

            else
                SouthWest

        SouthWest ->
            SouthWest

        West ->
            if theTailBeforeThisOne.y > currentTail.y then
                NorthWest

            else
                SouthWest

        NorthWest ->
            NorthWest

        NoMovement ->
            NoMovement



--case headDirection of
--     Up ->
--         ( { x = head.x
--           , y = head.y - 1
--           }
--         , if oldTail.x < head.x then
--             NorthEast
--
--           else
--             NorthWest
--         )
--
--     Down ->
--         ( { x = head.x
--           , y = head.y + 1
--           }
--         , if oldTail.x < head.x then
--             SouthEast
--
--           else
--             SouthWest
--         )
--
--     Right ->
--         ( { y = head.y
--           , x = head.x - 1
--           }
--         , if oldTail.y < head.y then
--             NorthWest
--
--           else
--             SouthWest
--         )
--
--     Left ->
--         ( { y = head.y
--           , x = head.x + 1
--           }
--         , if oldTail.y < head.y then
--             NorthEast
--
--           else
--             SouthEast
--         )


type TailMoveDirection
    = North
    | NorthEast
    | East
    | SouthEast
    | South
    | SouthWest
    | West
    | NorthWest
    | NoMovement


initialStateAndHistory : StateAndHistory
initialStateAndHistory =
    { state =
        { head = { x = 0, y = 0 }
        , firstTail = { x = 0, y = 0 }
        , tail = List.repeat 8 { x = 0, y = 0 }
        }
    , history = [ { x = 0, y = 0 } ]
    }


performSteps : List Command -> StateAndHistory
performSteps commands =
    List.foldl executeCommand
        initialStateAndHistory
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

                tailMoveDirection : TailMoveDirection
                tailMoveDirection =
                    findTailMoveDirection command.direction headLocation state.firstTail

                firstTailLocation : Coordinate
                firstTailLocation =
                    tailMoveDirectionToCoordinates tailMoveDirection state.firstTail

                newTail : List Coordinate
                newTail =
                    updateTail tailMoveDirection firstTailLocation state.tail
            in
            { state =
                { head = headLocation
                , firstTail = firstTailLocation
                , tail = newTail
                }
            , history =
                case List.Extra.last newTail of
                    Just last ->
                        last :: history

                    _ ->
                        history
            }
    in
    List.range 1 command.number
        |> List.foldl (\_ b -> step b) { state = initial.state, history = initial.history }


tailMoveDirectionToCoordinates : TailMoveDirection -> Coordinate -> Coordinate
tailMoveDirectionToCoordinates tailMoveDirection { x, y } =
    case tailMoveDirection of
        North ->
            { x = x
            , y = y + 1
            }

        NorthEast ->
            { x = x + 1
            , y = y + 1
            }

        East ->
            { x = x + 1
            , y = y
            }

        SouthEast ->
            { x = x + 1
            , y = y - 1
            }

        South ->
            { x = x
            , y = y - 1
            }

        SouthWest ->
            { x = x - 1
            , y = y - 1
            }

        West ->
            { x = x - 1
            , y = y
            }

        NorthWest ->
            { x = x - 1
            , y = y + 1
            }

        NoMovement ->
            { x = x
            , y = y
            }


countTailPositions : StateAndHistory -> Int
countTailPositions { history } =
    history
        |> List.map (\coordinate -> ( coordinate.x, coordinate.y ))
        |> Set.fromList
        |> Set.size


puzzleInput =
    String.trim """
R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20
"""
