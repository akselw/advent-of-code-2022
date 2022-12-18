module Day10Part2 exposing (main)

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
        |> List.filterMap toInstruction
        |> executeInstructions
        |> registerValuesToDrawing
        |> drawingValuesToHtml


type Instruction
    = AddX Int
    | NoOp


toInstruction : String -> Maybe Instruction
toInstruction string =
    case String.words string of
        "addx" :: valueString :: [] ->
            valueString
                |> String.toInt
                |> Maybe.map AddX

        "noop" :: [] ->
            Just NoOp

        _ ->
            Nothing


executeInstructions : List Instruction -> List Int
executeInstructions instructions =
    let
        helper instruction state =
            case instruction of
                AddX v ->
                    let
                        newRegisterValue =
                            state.registerValue + v
                    in
                    { reversedHistory = newRegisterValue :: state.registerValue :: state.reversedHistory
                    , registerValue = newRegisterValue
                    }

                NoOp ->
                    { state
                        | reversedHistory = state.registerValue :: state.reversedHistory
                    }

        { reversedHistory } =
            List.foldl helper { registerValue = 1, reversedHistory = [ 1 ] } instructions
    in
    List.reverse reversedHistory


findSignalStrengths : List Int -> List Int
findSignalStrengths registerValues =
    let
        helper index registerValue signalStrengths =
            if index == 20 || index == 60 || index == 100 || index == 140 || index == 180 || index == 220 then
                index * registerValue :: signalStrengths

            else
                signalStrengths
    in
    registerValues
        |> List.Extra.indexedFoldl helper []
        |> List.reverse


registerValuesToDrawing : List Int -> List Bool
registerValuesToDrawing registerValues =
    let
        calculatePixelOnOrOff : Int -> { position : Int, reversedDrawing : List Bool } -> { position : Int, reversedDrawing : List Bool }
        calculatePixelOnOrOff registerValue { position, reversedDrawing } =
            let
                positionInRow : Int
                positionInRow =
                    remainderBy 40 position

                pixelShouldBeDrawn : Bool
                pixelShouldBeDrawn =
                    (positionInRow == registerValue - 1 || positionInRow == registerValue || positionInRow == registerValue + 1)
                        |> Debug.log ("position: " ++ String.fromInt position ++ ", registerValue: " ++ String.fromInt registerValue ++ ", draw: ")
            in
            { position = position + 1
            , reversedDrawing = pixelShouldBeDrawn :: reversedDrawing
            }
    in
    List.foldl calculatePixelOnOrOff
        { position = 0, reversedDrawing = [] }
        registerValues
        |> .reversedDrawing
        |> List.reverse


drawingValuesToHtml : List Bool -> Html msg
drawingValuesToHtml drawingValues =
    drawingValues
        |> List.Extra.groupsOf 40
        |> List.map drawRow
        |> div []


drawRow : List Bool -> Html msg
drawRow drawingValues =
    drawingValues
        |> List.map
            (\shouldDrawPixel ->
                if shouldDrawPixel then
                    "#"

                else
                    "."
            )
        |> List.map text
        |> div []


puzzleInput =
    String.trim """
addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop
"""
