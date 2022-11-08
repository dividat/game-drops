module Drops.Metrics exposing
    ( Chain(..)
    , Metrics
    , init
    , update
    )

{-| Scoring as in Puyo Puyo 1992
<https://puyonexus.com/wiki/Scoring#Scoring_Formula>
-}

import Drops.Board exposing (Group(..))
import Drops.Cell as Cell
import List.Extra


type Chain
    = Chain (List (List Group))


type alias Metrics =
    { points : Int
    , longestChain : Int
    }


init : Metrics
init =
    { points = 0
    , longestChain = 0
    }


update : Chain -> Metrics -> Metrics
update chain metrics =
    let
        chainLength =
            chain
                |> (\(Chain c) -> List.length c)
    in
    { points = metrics.points + chainScore chain
    , longestChain = max metrics.longestChain chainLength
    }


chainScore : Chain -> Int
chainScore (Chain chain) =
    chain
        |> List.indexedMap groupsScore
        |> List.sum


groupsScore : Int -> List Group -> Int
groupsScore index groups =
    let
        cc =
            cellsCleared groups

        cp =
            chainPower index

        cb =
            colorBonus groups

        gb =
            groupBonus groups
    in
    (10 * cc) * ((cp + cb + gb) |> max 1 |> min 999)


cellsCleared : List Group -> Int
cellsCleared groups =
    groups
        |> List.map (\(Group g) -> g)
        |> List.concat
        |> List.length


chainPower : Int -> Int
chainPower index =
    [ 0, 8, 16, 32, 64, 128, 256, 512 ]
        |> List.Extra.getAt index
        |> Maybe.withDefault 999


colorBonus : List Group -> Int
colorBonus groups =
    let
        numColors =
            groups
                |> List.concatMap (\(Group g) -> g)
                |> List.map (.color >> Cell.colorToCss)
                |> List.Extra.unique
                |> List.length
    in
    case numColors of
        2 ->
            3

        3 ->
            6

        4 ->
            12

        5 ->
            24

        _ ->
            0


groupBonus : List Group -> Int
groupBonus groups =
    groups
        |> List.map
            (\(Group group) ->
                let
                    numCells =
                        List.length group
                in
                if numCells >= 11 then
                    10

                else if numCells > 4 then
                    numCells - 3

                else
                    0
            )
        |> List.sum
