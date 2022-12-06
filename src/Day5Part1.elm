module Day5Part1 exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import List.Extra
import Set


main =
    puzzleInput
        |> toCratePlan
        |> Debug.toString
        |> text


type alias CratePlan =
    { startingPoint : CrateState
    , steps : String
    }


type alias CrateState =
    Array (List Char)


toCratePlan : String -> Maybe CratePlan
toCratePlan string =
    case String.split "\n\n" string of
        startingPoint :: steps :: [] ->
            Maybe.map
                (\crateState ->
                    { startingPoint = crateState
                    , steps = steps
                    }
                )
                (toCrateState startingPoint)

        _ ->
            Nothing


toCrateState : String -> Maybe CrateState
toCrateState startingPoint =
    startingPoint
        |> String.lines
        |> List.Extra.unconsLast
        |> Maybe.map fromSplitToCrateState


fromSplitToCrateState : ( String, List String ) -> CrateState
fromSplitToCrateState ( last, startingList ) =
    Debug.log "t"
        (startingList
            |> List.map String.toList
            |> List.map toCrateRows
            |> List.Extra.transpose
        )
        |> List.map (List.filterMap identity)
        |> Array.fromList


type alias Stack =
    List Char


toCrateRows : List Char -> List (Maybe Char)
toCrateRows chars =
    case chars of
        '[' :: char :: ']' :: [] ->
            [ Just char ]

        ' ' :: ' ' :: ' ' :: [] ->
            [ Nothing ]

        '[' :: char :: ']' :: ' ' :: rest ->
            Just char :: toCrateRows rest

        ' ' :: ' ' :: ' ' :: ' ' :: rest ->
            Nothing :: toCrateRows rest

        _ ->
            []


puzzleInput : String
puzzleInput =
    "    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 \n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2"


puzzleInputWithoutWhiteSpace =
    """
    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
"""
