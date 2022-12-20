module Day11Part1 exposing (main)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, p, text)
import Html.Events exposing (onClick)
import List.Extra
import Set


main =
    puzzleInput
        |> String.split "\n\n"
        |> List.filterMap parseMonkeyInfo
        |> toMonkeyDict
        |> performRoundnNumberOfRounds 20
        |> Dict.values
        |> List.map .itemsInspected
        |> List.sort
        |> List.reverse
        |> List.take 2
        |> List.product
        |> Debug.toString
        |> text


type alias MonkeyInfo =
    { items : List Int
    , itemsInspected : Int
    , operation : Operation
    , testDivisibleBy : Int
    , throwToMonkeyIfTrue : Int
    , throwToMonkeyIfFalse : Int
    }


type Operation
    = MultiplyByOld
    | MultiplyByNumber Int
    | AddOld
    | AddNumber Int


parseMonkeyInfo : String -> Maybe MonkeyInfo
parseMonkeyInfo string =
    case String.lines string of
        monkeyNumber :: startingItemsString :: operationString :: testString :: ifTrueString :: ifFalseString :: [] ->
            Maybe.map4
                (MonkeyInfo (parseStartingItems startingItemsString) 0)
                (parseOperation operationString)
                (parseTestDivisibleBy testString)
                (parseThrowToMonkeyIfTrue ifTrueString)
                (parseThrowToMonkeyIfFalse ifFalseString)

        _ ->
            Nothing


parseStartingItems : String -> List Int
parseStartingItems string =
    string
        |> String.trim
        |> String.dropLeft 16
        |> String.split ", "
        |> List.filterMap String.toInt


parseOperation : String -> Maybe Operation
parseOperation string =
    let
        splittedOperation =
            string
                |> String.trim
                |> String.dropLeft 21
                |> String.words
    in
    case splittedOperation of
        "*" :: "old" :: [] ->
            Just MultiplyByOld

        "*" :: numberString :: [] ->
            numberString
                |> String.toInt
                |> Maybe.map MultiplyByNumber

        "+" :: "old" :: [] ->
            Just AddOld

        "+" :: numberString :: [] ->
            numberString
                |> String.toInt
                |> Maybe.map AddNumber

        _ ->
            Nothing


parseTestDivisibleBy : String -> Maybe Int
parseTestDivisibleBy string =
    string
        |> String.split "divisible by "
        |> List.Extra.getAt 1
        |> Maybe.andThen String.toInt


parseThrowToMonkeyIfTrue : String -> Maybe Int
parseThrowToMonkeyIfTrue string =
    string
        |> String.split "throw to monkey "
        |> List.Extra.getAt 1
        |> Maybe.andThen String.toInt


parseThrowToMonkeyIfFalse : String -> Maybe Int
parseThrowToMonkeyIfFalse string =
    string
        |> String.split "throw to monkey "
        |> List.Extra.getAt 1
        |> Maybe.andThen String.toInt


toMonkeyDict : List MonkeyInfo -> Dict Int MonkeyInfo
toMonkeyDict monkeyInfos =
    monkeyInfos
        |> List.indexedMap Tuple.pair
        |> Dict.fromList


performRoundnNumberOfRounds : Int -> Dict Int MonkeyInfo -> Dict Int MonkeyInfo
performRoundnNumberOfRounds n monkeyDict =
    List.range 1 n
        |> List.foldl (\_ -> performRound) monkeyDict


performRound : Dict Int MonkeyInfo -> Dict Int MonkeyInfo
performRound monkeyDict =
    monkeyDict
        |> Dict.keys
        |> List.sort
        |> List.foldl performMonkeyTurn monkeyDict


type alias MoveItemToMonkey =
    { item : Int
    , toMonkey : Int
    }


worryLevelAfterOperation : Operation -> Int -> Int
worryLevelAfterOperation operation initialWorryLevel =
    case operation of
        MultiplyByOld ->
            initialWorryLevel * initialWorryLevel

        MultiplyByNumber int ->
            initialWorryLevel * int

        AddOld ->
            initialWorryLevel + initialWorryLevel

        AddNumber int ->
            initialWorryLevel + int


divideByThree : Int -> Int
divideByThree int =
    int // 3


conditionIsMet : Int -> Int -> Bool
conditionIsMet dividend worryLevel =
    modBy dividend worryLevel == 0


performMonkeyTurn : Int -> Dict Int MonkeyInfo -> Dict Int MonkeyInfo
performMonkeyTurn key monkeyDict =
    let
        performInspection : MonkeyInfo -> Int -> MoveItemToMonkey
        performInspection monkeyInfo itemWorryLevel =
            let
                newWorryLevel =
                    itemWorryLevel
                        |> worryLevelAfterOperation monkeyInfo.operation
                        |> divideByThree
            in
            if conditionIsMet monkeyInfo.testDivisibleBy newWorryLevel then
                { item = newWorryLevel, toMonkey = monkeyInfo.throwToMonkeyIfTrue }

            else
                { item = newWorryLevel, toMonkey = monkeyInfo.throwToMonkeyIfFalse }

        helper : MonkeyInfo -> Dict Int MonkeyInfo
        helper monkeyInfo =
            monkeyInfo.items
                |> List.map (performInspection monkeyInfo)
                |> List.foldl moveItems monkeyDict
                |> Dict.update key emptyMonkeyItems
    in
    monkeyDict
        |> Dict.get key
        |> Maybe.map helper
        |> Maybe.withDefault monkeyDict


moveItems : MoveItemToMonkey -> Dict Int MonkeyInfo -> Dict Int MonkeyInfo
moveItems moveItemToMonkey monkeyDict =
    let
        updateItem : Maybe MonkeyInfo -> Maybe MonkeyInfo
        updateItem maybeMonkeyInfo =
            maybeMonkeyInfo
                |> Maybe.map (\monkeyInfoToUpdate -> { monkeyInfoToUpdate | items = List.append monkeyInfoToUpdate.items [ moveItemToMonkey.item ] })
    in
    monkeyDict
        |> Dict.update moveItemToMonkey.toMonkey updateItem


emptyMonkeyItems : Maybe MonkeyInfo -> Maybe MonkeyInfo
emptyMonkeyItems maybeMonkeyInfo =
    maybeMonkeyInfo
        |> Maybe.map
            (\monkeyInfo ->
                { monkeyInfo
                    | itemsInspected = monkeyInfo.itemsInspected + List.length monkeyInfo.items
                    , items = []
                }
            )


puzzleInput =
    String.trim """
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
"""
