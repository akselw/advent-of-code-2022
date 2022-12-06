module Day3Part1 exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Set


main =
    puzzleInput
        |> String.trim
        |> String.lines
        |> List.map toRucksackCompartments
        |> List.filterMap findDuplicateItem
        |> List.map itemCharToPriority
        |> List.sum
        |> Debug.toString
        |> text


type alias Rucksack =
    { firstCompartment : List Char
    , secondCompartment : List Char
    }


toRucksackCompartments : String -> Rucksack
toRucksackCompartments s =
    let
        partitionPoint =
            String.length s // 2
    in
    { firstCompartment =
        s
            |> String.toList
            |> List.take partitionPoint
    , secondCompartment =
        s
            |> String.toList
            |> List.drop partitionPoint
    }


findDuplicateItem : Rucksack -> Maybe Char
findDuplicateItem rucksack =
    Set.intersect
        (Set.fromList rucksack.firstCompartment)
        (Set.fromList rucksack.secondCompartment)
        |> Set.toList
        |> List.head


itemCharToPriority : Char -> Int
itemCharToPriority itemChar =
    if Char.isUpper itemChar then
        Char.toCode itemChar - 38

    else
        Char.toCode itemChar - 96


puzzleInput : String
puzzleInput =
    """
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
"""
