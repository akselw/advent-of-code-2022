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
        |> Maybe.map performSteps
        |> Maybe.map topCharsInStacks
        |> Debug.toString
        |> text


type alias CratePlan =
    { startingPoint : CrateState
    , steps : List SingleStep
    }


type alias Step =
    { fromStack : Int
    , toStack : Int
    , n : Int
    }


type alias CrateState =
    Array (List Char)


toCratePlan : String -> Maybe CratePlan
toCratePlan string =
    case String.split "\n\n" string of
        startingPoint :: stepString :: [] ->
            Maybe.map
                (\crateState ->
                    { startingPoint = crateState
                    , steps = toSteps stepString
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
    startingList
        |> List.map String.toList
        |> List.map toCrateRows
        |> List.Extra.transpose
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


toSteps : String -> List SingleStep
toSteps string =
    string
        |> String.lines
        |> List.filterMap toStep
        |> List.concatMap stepToSingleStep


toStep : String -> Maybe Step
toStep string =
    case String.split " " string of
        "move" :: nString :: "from" :: fromString :: "to" :: toString :: [] ->
            Maybe.map3 Step
                (String.toInt fromString)
                (String.toInt toString)
                (String.toInt nString)

        _ ->
            Nothing


type alias SingleStep =
    { fromStack : Int
    , toStack : Int
    }


stepToSingleStep : Step -> List SingleStep
stepToSingleStep step =
    List.repeat step.n
        { fromStack = step.fromStack
        , toStack = step.toStack
        }


performSteps : CratePlan -> CrateState
performSteps cratePlan =
    List.foldl performSingleStep cratePlan.startingPoint cratePlan.steps


performSingleStep : SingleStep -> CrateState -> CrateState
performSingleStep singleStep crateState =
    let
        elementToMove : Maybe Char
        elementToMove =
            crateState
                |> Array.get (singleStep.fromStack - 1)
                |> Maybe.andThen List.head
    in
    elementToMove
        |> Maybe.map (performSingleStepWithElement singleStep crateState)
        |> Maybe.withDefault crateState


performSingleStepWithElement : SingleStep -> CrateState -> Char -> CrateState
performSingleStepWithElement singleStep crateState char =
    crateState
        |> Array.indexedMap
            (\index stack ->
                if index == (singleStep.fromStack - 1) then
                    List.tail stack
                        |> Maybe.withDefault []

                else if index == (singleStep.toStack - 1) then
                    char :: stack

                else
                    stack
            )


topCharsInStacks : CrateState -> String
topCharsInStacks crateState =
    crateState
        |> Array.toList
        |> List.filterMap List.head
        |> String.fromList


puzzleInputTest : String
puzzleInputTest =
    "    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 \n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2"


puzzleInput =
    "    [M]             [Z]     [V]    \n    [Z]     [P]     [L]     [Z] [J]\n[S] [D]     [W]     [W]     [H] [Q]\n[P] [V] [N] [D]     [P]     [C] [V]\n[H] [B] [J] [V] [B] [M]     [N] [P]\n[V] [F] [L] [Z] [C] [S] [P] [S] [G]\n[F] [J] [M] [G] [R] [R] [H] [R] [L]\n[G] [G] [G] [N] [V] [V] [T] [Q] [F]\n 1   2   3   4   5   6   7   8   9 \n\nmove 6 from 9 to 3\nmove 2 from 2 to 1\nmove 1 from 8 to 2\nmove 3 from 7 to 2\nmove 7 from 6 to 9\nmove 1 from 9 to 5\nmove 3 from 5 to 7\nmove 6 from 8 to 6\nmove 1 from 7 to 8\nmove 6 from 6 to 5\nmove 4 from 5 to 8\nmove 9 from 2 to 9\nmove 1 from 2 to 3\nmove 3 from 1 to 3\nmove 3 from 5 to 1\nmove 10 from 3 to 5\nmove 4 from 4 to 6\nmove 2 from 7 to 6\nmove 2 from 6 to 9\nmove 6 from 8 to 6\nmove 1 from 4 to 3\nmove 1 from 4 to 5\nmove 1 from 4 to 1\nmove 2 from 3 to 1\nmove 1 from 3 to 7\nmove 8 from 1 to 9\nmove 1 from 1 to 2\nmove 1 from 2 to 7\nmove 6 from 6 to 3\nmove 7 from 3 to 5\nmove 14 from 5 to 6\nmove 2 from 1 to 3\nmove 5 from 5 to 8\nmove 5 from 8 to 1\nmove 2 from 7 to 1\nmove 5 from 6 to 9\nmove 8 from 9 to 3\nmove 13 from 9 to 3\nmove 7 from 1 to 4\nmove 6 from 4 to 1\nmove 22 from 3 to 1\nmove 1 from 9 to 3\nmove 2 from 6 to 1\nmove 1 from 3 to 4\nmove 7 from 9 to 8\nmove 2 from 1 to 7\nmove 2 from 3 to 2\nmove 2 from 6 to 9\nmove 2 from 7 to 8\nmove 1 from 3 to 6\nmove 9 from 8 to 6\nmove 1 from 2 to 4\nmove 8 from 1 to 2\nmove 1 from 9 to 4\nmove 3 from 4 to 1\nmove 1 from 4 to 6\nmove 10 from 6 to 5\nmove 5 from 2 to 9\nmove 6 from 9 to 3\nmove 2 from 5 to 3\nmove 2 from 9 to 7\nmove 7 from 5 to 8\nmove 5 from 6 to 2\nmove 3 from 3 to 7\nmove 3 from 3 to 5\nmove 4 from 5 to 8\nmove 1 from 3 to 5\nmove 6 from 6 to 8\nmove 1 from 5 to 7\nmove 9 from 8 to 9\nmove 1 from 3 to 1\nmove 7 from 2 to 7\nmove 9 from 7 to 6\nmove 2 from 2 to 3\nmove 7 from 9 to 3\nmove 9 from 6 to 8\nmove 7 from 3 to 4\nmove 2 from 7 to 6\nmove 4 from 4 to 5\nmove 3 from 5 to 6\nmove 2 from 7 to 4\nmove 5 from 4 to 7\nmove 13 from 8 to 4\nmove 2 from 9 to 4\nmove 2 from 8 to 7\nmove 6 from 7 to 5\nmove 6 from 4 to 2\nmove 1 from 7 to 5\nmove 3 from 2 to 7\nmove 1 from 7 to 8\nmove 3 from 2 to 4\nmove 2 from 3 to 9\nmove 2 from 7 to 2\nmove 6 from 5 to 4\nmove 3 from 6 to 2\nmove 2 from 6 to 9\nmove 5 from 2 to 9\nmove 12 from 4 to 8\nmove 3 from 9 to 2\nmove 12 from 1 to 5\nmove 4 from 4 to 6\nmove 12 from 8 to 9\nmove 2 from 6 to 5\nmove 1 from 4 to 8\nmove 1 from 4 to 1\nmove 3 from 2 to 1\nmove 2 from 6 to 7\nmove 1 from 5 to 9\nmove 2 from 1 to 4\nmove 10 from 5 to 1\nmove 2 from 7 to 3\nmove 18 from 9 to 7\nmove 8 from 7 to 2\nmove 1 from 9 to 6\nmove 1 from 6 to 7\nmove 10 from 7 to 9\nmove 1 from 4 to 2\nmove 19 from 1 to 5\nmove 8 from 5 to 9\nmove 3 from 8 to 4\nmove 2 from 5 to 2\nmove 2 from 3 to 6\nmove 10 from 5 to 2\nmove 4 from 1 to 2\nmove 2 from 9 to 2\nmove 1 from 1 to 6\nmove 2 from 5 to 6\nmove 1 from 8 to 7\nmove 1 from 5 to 8\nmove 1 from 5 to 6\nmove 18 from 2 to 5\nmove 5 from 2 to 1\nmove 6 from 5 to 8\nmove 1 from 8 to 9\nmove 2 from 2 to 4\nmove 1 from 2 to 6\nmove 2 from 7 to 6\nmove 1 from 4 to 1\nmove 4 from 8 to 5\nmove 1 from 2 to 9\nmove 2 from 8 to 3\nmove 1 from 3 to 6\nmove 1 from 4 to 8\nmove 1 from 8 to 9\nmove 10 from 5 to 7\nmove 5 from 5 to 1\nmove 2 from 4 to 1\nmove 3 from 7 to 6\nmove 12 from 1 to 4\nmove 8 from 9 to 5\nmove 6 from 7 to 4\nmove 1 from 7 to 9\nmove 4 from 4 to 3\nmove 1 from 1 to 7\nmove 3 from 9 to 5\nmove 2 from 3 to 1\nmove 1 from 7 to 6\nmove 8 from 4 to 7\nmove 1 from 7 to 6\nmove 7 from 6 to 4\nmove 2 from 1 to 3\nmove 1 from 7 to 1\nmove 1 from 3 to 7\nmove 1 from 1 to 6\nmove 4 from 9 to 3\nmove 5 from 4 to 6\nmove 12 from 6 to 2\nmove 3 from 9 to 4\nmove 8 from 2 to 6\nmove 2 from 9 to 6\nmove 8 from 5 to 6\nmove 4 from 5 to 8\nmove 14 from 6 to 3\nmove 11 from 4 to 9\nmove 2 from 2 to 7\nmove 8 from 3 to 9\nmove 11 from 3 to 6\nmove 14 from 9 to 1\nmove 7 from 1 to 3\nmove 2 from 9 to 5\nmove 2 from 2 to 8\nmove 6 from 7 to 5\nmove 1 from 9 to 8\nmove 13 from 6 to 3\nmove 4 from 6 to 8\nmove 3 from 1 to 6\nmove 5 from 5 to 8\nmove 7 from 8 to 7\nmove 2 from 1 to 8\nmove 1 from 4 to 1\nmove 4 from 8 to 9\nmove 8 from 7 to 5\nmove 1 from 8 to 1\nmove 4 from 9 to 3\nmove 1 from 4 to 5\nmove 5 from 5 to 2\nmove 1 from 8 to 9\nmove 1 from 8 to 6\nmove 2 from 6 to 2\nmove 4 from 8 to 6\nmove 4 from 1 to 8\nmove 4 from 8 to 5\nmove 1 from 9 to 8\nmove 1 from 2 to 3\nmove 4 from 6 to 1\nmove 1 from 8 to 2\nmove 3 from 5 to 4\nmove 4 from 2 to 5\nmove 1 from 7 to 9\nmove 1 from 2 to 6\nmove 3 from 1 to 8\nmove 2 from 4 to 5\nmove 2 from 6 to 1\nmove 3 from 8 to 9\nmove 4 from 9 to 2\nmove 1 from 7 to 1\nmove 1 from 6 to 7\nmove 4 from 1 to 6\nmove 1 from 7 to 4\nmove 6 from 2 to 8\nmove 2 from 4 to 8\nmove 1 from 9 to 5\nmove 3 from 6 to 2\nmove 1 from 6 to 4\nmove 7 from 3 to 5\nmove 2 from 8 to 1\nmove 3 from 2 to 8\nmove 6 from 8 to 5\nmove 17 from 5 to 3\nmove 2 from 1 to 6\nmove 3 from 8 to 3\nmove 1 from 9 to 5\nmove 11 from 5 to 2\nmove 40 from 3 to 5\nmove 11 from 2 to 7\nmove 4 from 7 to 8\nmove 1 from 8 to 9\nmove 1 from 3 to 5\nmove 1 from 4 to 8\nmove 19 from 5 to 8\nmove 7 from 7 to 8\nmove 16 from 5 to 2\nmove 6 from 5 to 8\nmove 1 from 5 to 8\nmove 1 from 9 to 4\nmove 1 from 6 to 1\nmove 1 from 4 to 7\nmove 1 from 6 to 9\nmove 1 from 1 to 7\nmove 1 from 7 to 3\nmove 1 from 7 to 2\nmove 1 from 9 to 8\nmove 1 from 3 to 4\nmove 1 from 4 to 6\nmove 14 from 2 to 9\nmove 24 from 8 to 4\nmove 8 from 8 to 3\nmove 1 from 6 to 3\nmove 16 from 4 to 1\nmove 3 from 8 to 4\nmove 3 from 3 to 8\nmove 4 from 3 to 4\nmove 1 from 3 to 9\nmove 13 from 9 to 4\nmove 16 from 1 to 8\nmove 8 from 8 to 1\nmove 3 from 1 to 7\nmove 1 from 8 to 6\nmove 1 from 3 to 8\nmove 10 from 8 to 5\nmove 5 from 5 to 2\nmove 3 from 8 to 9\nmove 1 from 8 to 9\nmove 1 from 4 to 5\nmove 5 from 2 to 6\nmove 3 from 5 to 2\nmove 1 from 6 to 1\nmove 5 from 1 to 5\nmove 1 from 1 to 5\nmove 2 from 7 to 3\nmove 2 from 3 to 2\nmove 1 from 5 to 7\nmove 7 from 5 to 3\nmove 5 from 9 to 5\nmove 2 from 7 to 9\nmove 4 from 5 to 6\nmove 2 from 9 to 8\nmove 2 from 2 to 4\nmove 5 from 3 to 5\nmove 1 from 3 to 2\nmove 7 from 4 to 9\nmove 1 from 8 to 1\nmove 1 from 2 to 1\nmove 9 from 4 to 6\nmove 2 from 1 to 8\nmove 1 from 3 to 9\nmove 2 from 8 to 6\nmove 13 from 4 to 6\nmove 1 from 8 to 7\nmove 2 from 9 to 6\nmove 3 from 5 to 7\nmove 3 from 2 to 5\nmove 3 from 2 to 6\nmove 5 from 6 to 2\nmove 4 from 2 to 5\nmove 4 from 5 to 7\nmove 5 from 5 to 7\nmove 7 from 9 to 6\nmove 6 from 7 to 2\nmove 22 from 6 to 5\nmove 10 from 5 to 8\nmove 7 from 5 to 4\nmove 8 from 8 to 5\nmove 18 from 6 to 2\nmove 5 from 7 to 5\nmove 1 from 8 to 2\nmove 6 from 5 to 1\nmove 7 from 4 to 2\nmove 4 from 1 to 5\nmove 1 from 7 to 9\nmove 1 from 8 to 6\nmove 1 from 7 to 8\nmove 10 from 5 to 9\nmove 12 from 2 to 1\nmove 8 from 5 to 2\nmove 19 from 2 to 9\nmove 1 from 6 to 8\nmove 13 from 9 to 3\nmove 8 from 1 to 2\nmove 5 from 1 to 3\nmove 10 from 2 to 1\nmove 7 from 2 to 5\nmove 3 from 5 to 7\nmove 4 from 1 to 3\nmove 1 from 2 to 3\nmove 3 from 1 to 2\nmove 1 from 8 to 6\nmove 2 from 7 to 5\nmove 4 from 1 to 3\nmove 6 from 5 to 4\nmove 2 from 2 to 1\nmove 1 from 2 to 9\nmove 6 from 4 to 5\nmove 5 from 5 to 9\nmove 1 from 6 to 8\nmove 1 from 5 to 1\nmove 6 from 9 to 2\nmove 5 from 2 to 4\nmove 3 from 1 to 6\nmove 2 from 4 to 7\nmove 22 from 3 to 9\nmove 1 from 8 to 4\nmove 2 from 4 to 3\nmove 2 from 6 to 1\nmove 2 from 1 to 5\nmove 1 from 6 to 7\nmove 1 from 7 to 4\nmove 6 from 3 to 7\nmove 1 from 2 to 4\nmove 8 from 7 to 3\nmove 1 from 4 to 5\nmove 1 from 7 to 9\nmove 5 from 3 to 6\nmove 1 from 8 to 4\nmove 4 from 3 to 2\nmove 32 from 9 to 3\nmove 3 from 6 to 7\nmove 5 from 9 to 3\nmove 1 from 9 to 7\nmove 2 from 9 to 2\nmove 2 from 4 to 3\nmove 2 from 5 to 4\nmove 5 from 3 to 2\nmove 3 from 7 to 8\nmove 1 from 7 to 2\nmove 1 from 8 to 5\nmove 1 from 3 to 4\nmove 5 from 4 to 5\nmove 4 from 5 to 2\nmove 3 from 5 to 7\nmove 1 from 7 to 5\nmove 1 from 6 to 5\nmove 2 from 8 to 5\nmove 15 from 2 to 4\nmove 3 from 5 to 6\nmove 4 from 6 to 5\nmove 2 from 5 to 2\nmove 1 from 2 to 4\nmove 25 from 3 to 9\nmove 2 from 5 to 2\nmove 11 from 9 to 2\nmove 13 from 2 to 1\nmove 4 from 4 to 7\nmove 12 from 9 to 8\nmove 6 from 7 to 8\nmove 7 from 4 to 7\nmove 7 from 7 to 8\nmove 1 from 5 to 1\nmove 5 from 4 to 3\nmove 2 from 2 to 1\nmove 2 from 9 to 5\nmove 7 from 1 to 7\nmove 1 from 1 to 4\nmove 12 from 3 to 2\nmove 1 from 3 to 9\nmove 1 from 1 to 3\nmove 1 from 9 to 1\nmove 7 from 7 to 2\nmove 1 from 4 to 7\nmove 2 from 8 to 7\nmove 7 from 1 to 2\nmove 1 from 3 to 4\nmove 26 from 2 to 1\nmove 4 from 8 to 1\nmove 3 from 1 to 6\nmove 1 from 6 to 3\nmove 1 from 6 to 9\nmove 1 from 3 to 8\nmove 20 from 1 to 3\nmove 1 from 9 to 7\nmove 4 from 7 to 1\nmove 1 from 5 to 3\nmove 4 from 3 to 5\nmove 1 from 6 to 2\nmove 6 from 3 to 2\nmove 8 from 1 to 4\nmove 1 from 1 to 5\nmove 3 from 1 to 4\nmove 7 from 2 to 4\nmove 10 from 3 to 8\nmove 4 from 4 to 3\nmove 12 from 4 to 7\nmove 3 from 3 to 1\nmove 2 from 4 to 3\nmove 2 from 8 to 1\nmove 6 from 8 to 9\nmove 5 from 9 to 6\nmove 1 from 9 to 3\nmove 3 from 8 to 7\nmove 10 from 8 to 5\nmove 4 from 8 to 7\nmove 9 from 7 to 9\nmove 4 from 8 to 4\nmove 2 from 4 to 3\nmove 3 from 1 to 7\nmove 11 from 7 to 4\nmove 6 from 4 to 8\nmove 1 from 7 to 3\nmove 4 from 5 to 1\nmove 5 from 3 to 6\nmove 5 from 9 to 4\nmove 1 from 9 to 8\nmove 10 from 4 to 8\nmove 5 from 1 to 2\nmove 1 from 7 to 6\nmove 9 from 6 to 3\nmove 7 from 8 to 7\nmove 3 from 4 to 1\nmove 2 from 2 to 1\nmove 9 from 8 to 3\nmove 10 from 5 to 8\nmove 18 from 3 to 9\nmove 1 from 7 to 8\nmove 1 from 5 to 3\nmove 4 from 8 to 3\nmove 2 from 6 to 3\nmove 6 from 7 to 2\nmove 1 from 5 to 3\nmove 1 from 1 to 9\nmove 10 from 3 to 9\nmove 4 from 1 to 8\nmove 13 from 8 to 1\nmove 3 from 1 to 8\nmove 3 from 2 to 4\nmove 5 from 2 to 6\nmove 5 from 6 to 4\nmove 28 from 9 to 2\nmove 2 from 9 to 5\nmove 2 from 5 to 2\nmove 1 from 3 to 7\nmove 2 from 1 to 4\nmove 3 from 8 to 3\nmove 1 from 9 to 4\nmove 3 from 4 to 6\nmove 2 from 3 to 7\nmove 8 from 1 to 5\nmove 3 from 7 to 6\nmove 14 from 2 to 8\nmove 1 from 9 to 1\nmove 6 from 5 to 6\nmove 4 from 2 to 5\nmove 9 from 8 to 2\nmove 4 from 8 to 4\nmove 7 from 2 to 4\nmove 12 from 4 to 3\nmove 5 from 4 to 7\nmove 5 from 7 to 4\nmove 1 from 8 to 7\nmove 1 from 4 to 5\nmove 2 from 5 to 4\nmove 1 from 5 to 8\nmove 1 from 5 to 9"


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
