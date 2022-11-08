module Drops.Board exposing
    ( EvaluationResult(..)
    , Group(..)
    , MoveResult(..)
    , Movement(..)
    , Pair
    , Pile(..)
    , applyMovement
    , cellsOfPair
    , columns
    , evaluatePile
    , genPair
    , hasBase
    , removeGroups
    , rows
    )

import Drops.Cell as Cell exposing (Cell)
import List.Extra
import Random
import Random.List
import Time_ exposing (Time_)
import Uuid


columns : Int
columns =
    6


rows : Int
rows =
    12


type Movement
    = Down
    | Left
    | Right
    | Spin


type alias Pair =
    ( Cell, Cell )


type Pile
    = Pile (List Cell)


type Group
    = Group (List Cell)


genPair : Random.Generator Pair
genPair =
    let
        pickColor =
            Cell.colors
                |> Random.List.shuffle
                |> Random.map (List.head >> Maybe.withDefault Cell.Black)

        -- Center or just left of center if center is not an integer
        x =
            ceiling (toFloat columns / 2) - 1

        initCell y color id =
            { color = color
            , x = x
            , y = y
            , fallTo = Nothing
            , isPartOfGroup = False
            , id = id
            , isFresh = True
            }
    in
    Random.pair
        (Random.map2 (initCell -2) pickColor Uuid.uuidGenerator)
        (Random.map2 (initCell -1) pickColor Uuid.uuidGenerator)



--- Helpers


getCell : Int -> Int -> Pile -> Maybe Cell
getCell x y (Pile cells) =
    cells
        |> List.Extra.find (\c -> floor c.y == y && c.x == x)


pileHasCellAt : Int -> Int -> Pile -> Bool
pileHasCellAt x y pile =
    getCell x y pile
        |> (not << (==) Nothing)


markCellsToFall : Pile -> Pile
markCellsToFall ((Pile cells) as pile) =
    let
        markCellToFall : Cell -> Cell
        markCellToFall cell =
            if (not <| floor cell.y == rows - 1) && (not <| pileHasCellAt cell.x (floor <| cell.y + 1) pile) then
                { cell | fallTo = Just <| floor <| cell.y + 1 }

            else
                cell
    in
    cells
        |> List.map markCellToFall
        |> Pile


markMatureCells : List Cell -> List Cell
markMatureCells =
    List.map (\c -> { c | isFresh = False })



-- Consolidation


type EvaluationResult
    = Evaluated
    | GroupsDetected (List Group) Pile
    | ContinueConsolidation Pile


evaluatePile : Time_ -> Pile -> EvaluationResult
evaluatePile dt (Pile cells) =
    let
        needConsolidation =
            cells |> List.filter (\cell -> cell.fallTo /= Nothing)
    in
    case needConsolidation of
        [] ->
            let
                groups =
                    findGroups (Pile cells)

                groupedCells : List Cell
                groupedCells =
                    groups
                        |> List.map (\(Group groupCells) -> groupCells)
                        |> List.concat
            in
            case groupedCells of
                [] ->
                    Evaluated

                _ ->
                    cells
                        |> List.map
                            (\c ->
                                if List.member c groupedCells then
                                    { c | isPartOfGroup = True }

                                else
                                    c
                            )
                        |> Pile
                        |> GroupsDetected groups

        _ ->
            let
                updateYAndTarget : Cell -> Int -> Cell
                updateYAndTarget cell t =
                    let
                        delta =
                            dt * 0.015

                        newY =
                            min (cell.y + delta) (toFloat t)
                    in
                    if newY >= toFloat t then
                        { cell
                            | y = toFloat (floor newY)
                            , fallTo =
                                Nothing
                        }

                    else
                        { cell
                            | y = newY
                            , fallTo =
                                Just t
                        }

                updateCell cell =
                    cell.fallTo
                        |> Maybe.map (updateYAndTarget cell)
                        |> Maybe.withDefault cell
            in
            cells
                |> List.map updateCell
                |> Pile
                |> markCellsToFall
                |> ContinueConsolidation


removeGroups : Pile -> Pile
removeGroups (Pile cells) =
    List.filter (not << .isPartOfGroup) cells
        |> Pile
        |> markCellsToFall


findGroups : Pile -> List Group
findGroups ((Pile cells) as pile) =
    let
        search cell visited acc =
            let
                neighbors : List Cell
                neighbors =
                    [ getCell (cell.x + 1) (floor cell.y) pile
                    , getCell (cell.x - 1) (floor cell.y) pile
                    , getCell cell.x (floor cell.y + 1) pile
                    , getCell cell.x (floor cell.y - 1) pile
                    ]
                        |> List.filterMap identity
                        |> List.filter (\{ color } -> color == cell.color)
                        |> List.Extra.filterNot (\n -> List.member n visited)

                group =
                    neighbors
                        |> List.foldl
                            (\neighborCell acc_ ->
                                acc_ ++ search neighborCell (neighborCell :: visited) acc_
                            )
                            acc
            in
            cell :: group |> List.Extra.uniqueBy (.id >> Uuid.toString)

        { groups } =
            cells
                |> List.foldl
                    (\cell acc ->
                        if List.member cell acc.visited then
                            acc

                        else
                            let
                                group =
                                    search cell (cell :: acc.visited) []
                            in
                            { groups = group :: acc.groups
                            , visited = acc.visited ++ group
                            }
                    )
                    { groups = [], visited = [] }

        result =
            groups
                |> List.filter (\c -> List.length c > 3)
                |> List.map Group
    in
    result


cellsOfPair : ( b, b ) -> List b
cellsOfPair ( c1, c2 ) =
    [ c1, c2 ]



-- Movement


{-| Has a pair a base below ?
-}
hasBase : Pair -> Pile -> Bool
hasBase pair pile =
    case applyMovement Down pair pile of
        EvaluatePile _ ->
            True

        _ ->
            False


applyMovement : Movement -> Pair -> Pile -> MoveResult
applyMovement movement pair ((Pile cells) as pile) =
    let
        hasCellBelow c =
            pile
                |> pileHasCellAt c.x (floor c.y + 1)

        bottomed c =
            floor c.y == rows - 1 || hasCellBelow c

        ( cell1, cell2 ) =
            pair

        needsEvaluation =
            List.any bottomed [ cell1, cell2 ]
    in
    case ( movement, needsEvaluation ) of
        ( Down, True ) ->
            EvaluatePile <| markCellsToFall (Pile <| cell1 :: cell2 :: markMatureCells cells)

        _ ->
            ContinuePlaying <|
                case movement of
                    Down ->
                        moveDown pair
                            |> Just

                    Left ->
                        pair |> moveHorizontally -1 pile

                    Right ->
                        pair |> moveHorizontally 1 pile

                    Spin ->
                        pair |> spin pile


{-| Spin around the cell first entering the screen.
-}
spin : Pile -> Pair -> Maybe Pair
spin pile ( c1, c2 ) =
    let
        newC1 =
            if c1.y < c2.y && c1.x == c2.x then
                { c1
                    | x = c1.x + 1
                    , y = c1.y + 1
                }

            else if c1.y > c2.y && c1.x == c2.x then
                { c1
                    | x = c1.x - 1
                    , y = c1.y - 1
                }

            else if c1.y == c2.y && c1.x < c2.x then
                { c1
                    | x = c1.x + 1
                    , y = c1.y - 1
                }

            else
                { c1
                    | x = c1.x - 1
                    , y = c1.y + 1
                }

        -- adjust position if we collide with playing field boundary after spin
        adjustX =
            if newC1.x < 0 then
                moveCellX 1

            else if newC1.x >= columns then
                moveCellX -1

            else
                identity

        adjustY =
            if newC1.y < 0 then
                moveCellY 1

            else if floor newC1.y >= rows then
                moveCellY -1

            else
                identity

        adjustPos =
            adjustX >> adjustY

        newPair =
            Tuple.mapBoth adjustPos adjustPos ( newC1, c2 )
    in
    -- if we spin into some cells in the pile then return the pair untouched
    if isPairBlocked newPair pile then
        Nothing

    else
        Just newPair


type MoveResult
    = ContinuePlaying (Maybe Pair)
    | EvaluatePile Pile


moveDown : Pair -> Pair
moveDown pair =
    let
        moveY cell =
            { cell | y = cell.y + 1 }
    in
    Tuple.mapBoth moveY moveY pair


moveCellY : Int -> Cell -> Cell
moveCellY amount c =
    { c | y = c.y + toFloat amount }


moveCellX : Int -> Cell -> Cell
moveCellX amount c =
    { c | x = c.x + amount }


moveHorizontally : Int -> Pile -> Pair -> Maybe Pair
moveHorizontally amount pile pair =
    let
        ( c1, c2 ) =
            Tuple.mapBoth (moveCellX amount) (moveCellX amount) pair

        isOutOfBounds c =
            c.x < 0 || c.x >= columns
    in
    if isOutOfBounds c1 || isOutOfBounds c2 || isPairBlocked ( c1, c2 ) pile then
        Nothing

    else
        Just ( c1, c2 )


{-| Check if a pair is free to be moved horizontally (no cells in the way).
-}
isPairBlocked : Pair -> Pile -> Bool
isPairBlocked ( c1, c2 ) pile =
    pileHasCellAt c1.x (floor c1.y) pile || pileHasCellAt c2.x (floor c2.y) pile
