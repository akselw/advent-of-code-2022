module Day3Part2 exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import List.Extra
import Set exposing (Set)


main =
    puzzleInput
        |> String.trim
        |> String.lines
        |> List.map toRucksackCompartments
        |> List.Extra.groupsOf 3
        |> List.filterMap findGroupBadge
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


findGroupBadge : List Rucksack -> Maybe Char
findGroupBadge rucksacks =
    case rucksacks of
        first :: second :: third :: [] ->
            Set.intersect (rucksackToSet first) (rucksackToSet second)
                |> Set.intersect (rucksackToSet third)
                |> Set.toList
                |> List.head

        _ ->
            Nothing


rucksackToSet : Rucksack -> Set Char
rucksackToSet rucksack =
    Set.union
        (Set.fromList rucksack.firstCompartment)
        (Set.fromList rucksack.secondCompartment)


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
