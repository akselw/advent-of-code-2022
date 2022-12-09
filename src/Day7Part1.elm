module Day7Part1 exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import List.Extra
import Set


main =
    puzzleInput
        |> String.lines
        |> List.filterMap toLine
        |> linesToFileStructure
        |> Debug.toString
        |> text


type Line
    = CdCommand String
    | CdToParentCommand
    | CdToRootCommand
    | LsCommand
    | FileListing String Int
    | DirectoryListing String


toLine : String -> Maybe Line
toLine string =
    case String.words string of
        "$" :: "cd" :: "/" :: [] ->
            Just CdToRootCommand

        "$" :: "cd" :: ".." :: [] ->
            Just CdToParentCommand

        "$" :: "cd" :: dirname :: [] ->
            Just (CdCommand dirname)

        "$" :: "ls" :: [] ->
            Just LsCommand

        "dir" :: dirName :: [] ->
            Just (DirectoryListing dirName)

        filesize :: filename :: [] ->
            filesize
                |> String.toInt
                |> Maybe.map (FileListing filename)

        _ ->
            Nothing


type FileStructure
    = Dir String (List FileStructure)
    | File String Int


type alias TraversingState =
    { reversedPath : List String
    , filesystem : FileStructure
    }


linesToFileStructure lines =
    let
        helper : Line -> TraversingState -> TraversingState
        helper line traversingState =
            (case line of
                CdCommand dirname ->
                    { traversingState
                        | reversedPath =
                            dirname :: traversingState.reversedPath
                    }

                CdToParentCommand ->
                    { traversingState
                        | reversedPath =
                            traversingState.reversedPath
                                |> List.tail
                                |> Maybe.withDefault []
                    }

                CdToRootCommand ->
                    { traversingState | reversedPath = [] }

                LsCommand ->
                    traversingState

                FileListing string int ->
                    { traversingState
                        | filesystem =
                            traversingState.reversedPath
                                |> List.reverse
                                |> addFileToNestedDirectory string int traversingState.filesystem
                    }

                DirectoryListing string ->
                    traversingState
            )
                |> Debug.log "Test"
    in
    lines
        |> List.foldl helper
            { reversedPath = []
            , filesystem = Dir "/" []
            }


addFileToNestedDirectory : String -> Int -> FileStructure -> List String -> FileStructure
addFileToNestedDirectory filename size filesystem path =
    case path of
        [] ->
            addFileToDirectory filename size filesystem

        currentDirName :: rest ->
            case filesystem of
                Dir dirname files ->
                    files
                        |> List.map
                            (\f ->
                                case f of
                                    Dir d _ ->
                                        if d == currentDirName then
                                            addFileToNestedDirectory filename size f rest

                                        else
                                            f

                                    _ ->
                                        f
                            )
                        |> Dir dirname

                File currentFilename filesize ->
                    File currentFilename filesize


addFileToDirectory : String -> Int -> FileStructure -> FileStructure
addFileToDirectory filename size filesystem =
    case filesystem of
        Dir dirname files ->
            Dir dirname (File filename size :: files)

        File _ _ ->
            filesystem


puzzleInput =
    String.trim """
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
"""
