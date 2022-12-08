module Day6Part2 exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import List.Extra
import Set


main =
    puzzleInput
        |> findMarkerPosition
        |> Debug.toString
        |> text


findMarkerPosition : String -> Maybe Int
findMarkerPosition string =
    let
        markerSize : Int
        markerSize =
            14

        helper : Int -> List Char -> Maybe Int
        helper index chars =
            let
                numberOfUniqueChars =
                    chars
                        |> List.take markerSize
                        |> Set.fromList
                        |> Set.size

                _ =
                    Debug.log "index" index
            in
            if numberOfUniqueChars == markerSize then
                Just index

            else
                case List.tail chars of
                    Just rest ->
                        helper (index + 1) rest

                    Nothing ->
                        Nothing
    in
    string
        |> String.toList
        |> helper markerSize


puzzleInput =
    String.trim """
mjqjpqmgbljsphdztnvjfqwrcgsmlb
"""
