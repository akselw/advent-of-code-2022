module Day7Part1 exposing (main)

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
        |> List.filterMap toLine
        |> linesToFileStructure
        |> toDirectorySizes
        |> List.filter (\size -> size <= 100000)
        |> List.sum
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
    = Dir (Dict String FileStructure)
    | File Int


type alias TraversingState =
    { reversedPath : List String
    , filesystem : FileStructure
    }


linesToFileStructure : List Line -> FileStructure
linesToFileStructure lines =
    let
        helper : Line -> TraversingState -> TraversingState
        helper line traversingState =
            case line of
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
    in
    lines
        |> List.foldl helper
            { reversedPath = []
            , filesystem = Dir Dict.empty
            }
        |> .filesystem


addFileToNestedDirectory : String -> Int -> FileStructure -> List String -> FileStructure
addFileToNestedDirectory filename size filesystem path =
    case path of
        [] ->
            case filesystem of
                Dir files ->
                    addFileToDirectory filename size files

                File s ->
                    File s

        currentDirName :: rest ->
            case filesystem of
                Dir files ->
                    files
                        |> Dict.update currentDirName
                            (\maybeF ->
                                case maybeF of
                                    Just (Dir subFiles) ->
                                        Just (addFileToNestedDirectory filename size (Dir subFiles) rest)

                                    Nothing ->
                                        Just (addFileToNestedDirectory filename size (Dir Dict.empty) rest)

                                    Just file ->
                                        Just file
                            )
                        |> Dir

                File filesize ->
                    File filesize


addFileToDirectory : String -> Int -> Dict String FileStructure -> FileStructure
addFileToDirectory filename size files =
    files
        |> Dict.insert filename (File size)
        |> Dir


toDirectorySizes : FileStructure -> List Int
toDirectorySizes fileStructure =
    case fileStructure of
        Dir files ->
            let
                totalSize : Int
                totalSize =
                    getSizeOfDirectoryContent files

                subFolderSizes : List Int
                subFolderSizes =
                    files
                        |> Dict.values
                        |> List.concatMap toDirectorySizes
            in
            totalSize :: subFolderSizes

        _ ->
            []


getSizeOfDirectoryContent : Dict String FileStructure -> Int
getSizeOfDirectoryContent files =
    files
        |> Dict.map
            (\key fileStructure ->
                case fileStructure of
                    Dir subFiles ->
                        getSizeOfDirectoryContent subFiles

                    File size ->
                        size
            )
        |> Dict.values
        |> List.sum


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
