module Drops exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Events
import Drops.Board as Board
    exposing
        ( EvaluationResult(..)
        , Group(..)
        , MoveResult(..)
        , Movement(..)
        , Pair
        , Pile(..)
        )
import Drops.Cell as Cell exposing (Cell)
import Drops.CssUtils exposing (px, scale)
import Drops.Metrics as Metrics exposing (Chain(..), Metrics)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Html.Keyed as HK
import Json.Decode as JD
import Particle exposing (Particle)
import Particle.System
import Random
import Random.Extra
import Random.Float
import Return exposing (Return)
import Svg as S
import Svg.Attributes as SA
import Task
import Time_ exposing (Time_)
import UI
import Util
import Uuid
import WebAudio
import Window


type alias GroupAnimationState =
    { groups : List Group
    , time : Time_
    }


type PlayState
    = Paused Pair
    | Playing Pair
    | WaitingForNext
    | Evaluating
        { groupAnimationState : Maybe GroupAnimationState
        , chain : Chain
        }
    | GameOver


type alias State =
    { nextPair : Pair
    , playState : PlayState
    , explosions : Explosions
    , pile : Pile
    , metrics : Metrics
    , time : Time_
    , lastMove : Time_
    , accelerate : Bool
    }


type alias Model =
    { state : Maybe State
    , windowSize : Maybe Window.Size
    }


type Msg
    = InitialState State
    | Resize Window.Size
    | Move Movement
    | Accelerate Bool
    | Next Pair
    | Tick Time_
    | ParticleMsg (Particle.System.Msg Explosion)
    | TogglePause


success1Sound : String
success1Sound =
    "sound/drops/success-1.opus"


success2Sound : String
success2Sound =
    "sound/drops/success-2.opus"


success3Sound : String
success3Sound =
    "sound/drops/success-3.opus"


success4Sound : String
success4Sound =
    "sound/drops/success-4.opus"


waterDropSound : String
waterDropSound =
    "sound/drops/water-drop.opus"


impactSound : String
impactSound =
    "sound/drops/impact.opus"


init : Return Msg Model
init =
    let
        toState : Pair -> Pair -> State
        toState p1 p2 =
            { nextPair = p1
            , playState = Paused p2
            , pile = Pile []
            , metrics = Metrics.init
            , time = 0
            , lastMove = 0
            , explosions = Particle.System.init (Random.initialSeed 0)
            , accelerate = False
            }
    in
    { state = Nothing
    , windowSize = Nothing
    }
        |> Return.singleton
        |> Return.command
            (Random.map2
                toState
                Board.genPair
                Board.genPair
                |> Random.generate InitialState
            )
        |> Return.command
            (Window.size |> Task.perform Resize)
        |> Return.command
            ([ waterDropSound, impactSound, success1Sound, success2Sound, success3Sound, success4Sound ] |> WebAudio.loadSounds)


keyDecoder : JD.Decoder String
keyDecoder =
    JD.field "key" JD.string


keyUpDecoder : JD.Decoder Msg
keyUpDecoder =
    keyDecoder
        |> JD.andThen
            (\key ->
                case key of
                    "ArrowDown" ->
                        Accelerate False |> JD.succeed

                    _ ->
                        JD.fail "Unsupported keyUp"
            )


keyDownDecoder : JD.Decoder Msg
keyDownDecoder =
    keyDecoder
        |> JD.andThen
            (\key ->
                case key of
                    "ArrowDown" ->
                        Accelerate True |> JD.succeed

                    "ArrowLeft" ->
                        Move Left |> JD.succeed

                    "ArrowRight" ->
                        Move Right |> JD.succeed

                    " " ->
                        Move Spin |> JD.succeed

                    "Enter" ->
                        TogglePause |> JD.succeed

                    _ ->
                        JD.fail "Unsupported keyDown"
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <|
        [ Window.resizes Resize
        , case model.state |> Maybe.map .playState of
            Just (Paused _) ->
                Sub.none

            Just GameOver ->
                Sub.none

            _ ->
                Browser.Events.onAnimationFrameDelta Tick
        , case Maybe.map .explosions model.state of
            Just system ->
                Particle.System.sub [] ParticleMsg system

            _ ->
                Sub.none
        , Browser.Events.onKeyDown keyDownDecoder
        , Browser.Events.onKeyUp keyUpDecoder
        ]


update : Msg -> Model -> Return Msg Model
update msg model =
    case ( msg, model.state ) of
        ( Resize windowSize, _ ) ->
            { model | windowSize = Just windowSize }
                |> Return.singleton

        ( InitialState state, Nothing ) ->
            { model | state = Just state }
                |> Return.singleton

        ( msg_, Just state ) ->
            updateState msg_ state
                |> Return.map (\newState -> { model | state = Just newState })

        _ ->
            model
                |> Return.singleton


checkOutOfTime : State -> State
checkOutOfTime state =
    if state.time >= gameDuration then
        { state | playState = GameOver }

    else
        state


tick : Time_ -> State -> Return Msg State
tick dt state =
    case state.playState of
        Playing _ ->
            state
                |> checkOutOfTime
                |> Return.singleton
                |> Return.andThen
                    (\state_ ->
                        let
                            period =
                                if state_.accelerate then
                                    50

                                else
                                    750
                        in
                        if state_.time - state_.lastMove >= period then
                            { state_ | lastMove = state_.time }
                                |> Return.singleton
                                |> Return.command (Util.send (Move Down))

                        else
                            state_
                                |> Return.singleton
                    )

        WaitingForNext ->
            state
                |> checkOutOfTime
                |> Return.singleton

        Evaluating ({ groupAnimationState, chain } as evaluationState) ->
            case groupAnimationState of
                Just animationState ->
                    if animationState.time > 1 * Time_.second then
                        state
                            |> burstGroups animationState.groups
                            |> Return.map (\state_ -> { state_ | playState = Evaluating { evaluationState | groupAnimationState = Nothing } })
                            |> Return.command
                                (WebAudio.playSounds
                                    [ case chain of
                                        Chain xs ->
                                            case List.length xs of
                                                1 ->
                                                    success1Sound

                                                2 ->
                                                    success2Sound

                                                3 ->
                                                    success3Sound

                                                _ ->
                                                    success4Sound
                                    ]
                                )

                    else
                        { state | playState = Evaluating { evaluationState | groupAnimationState = Just { animationState | time = animationState.time + dt } } }
                            |> Return.singleton

                Nothing ->
                    case Board.evaluatePile dt state.pile of
                        Evaluated ->
                            let
                                (Pile pileCells) =
                                    state.pile

                                isGameOver =
                                    List.any (\{ y } -> y < 0) pileCells
                            in
                            if isGameOver then
                                { state
                                    | playState = GameOver
                                }
                                    |> Return.singleton

                            else
                                { state
                                    | playState = WaitingForNext
                                    , metrics = Metrics.update evaluationState.chain state.metrics
                                    , accelerate = False
                                }
                                    |> Return.singleton
                                    |> Return.command
                                        (Board.genPair
                                            |> Random.generate Next
                                        )

                        GroupsDetected newGroups pile ->
                            { state
                                | pile = pile
                                , playState =
                                    Evaluating
                                        { evaluationState
                                            | chain =
                                                let
                                                    (Chain groups) =
                                                        evaluationState.chain
                                                in
                                                Chain (groups ++ [ newGroups ])
                                            , groupAnimationState = Just { time = 0, groups = newGroups }
                                        }
                            }
                                |> Return.singleton

                        ContinueConsolidation pile ->
                            { state | pile = pile }
                                |> Return.singleton
                                |> Return.effect_ (playSoundIfLanding { isConsolidation = True } state)

        Paused _ ->
            state
                |> Return.singleton

        GameOver ->
            state
                |> Return.singleton


updateState : Msg -> State -> Return Msg State
updateState msg state =
    case ( msg, state.playState ) of
        ( TogglePause, Playing pair ) ->
            { state | playState = Paused pair }
                |> Return.singleton

        ( TogglePause, Paused pair ) ->
            { state | playState = Playing pair }
                |> Return.singleton

        ( Move movement, Playing currentPair ) ->
            let
                feedback cmd state_ =
                    Cmd.batch
                        [ playSoundIfLanding { isConsolidation = False } state state_
                        , case movement of
                            Down ->
                                Cmd.none

                            _ ->
                                cmd
                        ]

                negativeFeedback =
                    feedback (WebAudio.playSound impactSound)

                positiveFeedback =
                    feedback Cmd.none
            in
            case Board.applyMovement movement currentPair state.pile of
                ContinuePlaying (Just newPair) ->
                    { state
                        | playState = Playing newPair
                    }
                        |> Return.singleton
                        |> Return.effect_ positiveFeedback

                ContinuePlaying Nothing ->
                    state
                        |> Return.singleton
                        |> Return.effect_ negativeFeedback

                EvaluatePile newPile ->
                    { state
                        | pile = newPile
                        , playState =
                            Evaluating
                                { groupAnimationState = Nothing
                                , chain = Chain []
                                }
                    }
                        |> Return.singleton
                        |> Return.effect_ positiveFeedback

        ( ParticleMsg subMsg, _ ) ->
            { state
                | explosions = Particle.System.update subMsg state.explosions
            }
                |> Return.singleton

        ( Tick dt, _ ) ->
            { state | time = state.time + dt }
                |> tick dt

        ( Next newPair, _ ) ->
            { state
                | playState = Playing state.nextPair
                , nextPair = newPair
            }
                |> Return.singleton

        ( Accelerate bool, _ ) ->
            { state | accelerate = bool }
                |> Return.singleton

        ( _, _ ) ->
            state
                |> Return.singleton


{-| Play sound if playable pair lands on a base below (bottom or other cell)
-}
playSoundIfLanding : { isConsolidation : Bool } -> State -> State -> Cmd Msg
playSoundIfLanding { isConsolidation } prevState nextState =
    case nextState.playState of
        Playing nextPair ->
            case prevState.playState of
                Playing prevPair ->
                    if not (Board.hasBase prevPair prevState.pile) && Board.hasBase nextPair nextState.pile then
                        WebAudio.playSound waterDropSound

                    else
                        Cmd.none

                _ ->
                    if Board.hasBase nextPair nextState.pile then
                        WebAudio.playSound waterDropSound

                    else
                        Cmd.none

        Evaluating _ ->
            -- It's safe to pass 0 as delta time here as we're not updating any state,
            -- only checking whether evaluation of the pile would result in some cells landing.
            -- This is also based on the assumption that `evaluatePile` first checks the pile before updating it,
            -- so when calculating the return values relevant here, the time value isn't used.
            case Board.evaluatePile 0 nextState.pile of
                ContinueConsolidation _ ->
                    Cmd.none

                _ ->
                    if isConsolidation then
                        WebAudio.playSound waterDropSound

                    else
                        Cmd.none

        _ ->
            Cmd.none



-- View


view : Model -> H.Html Msg
view model =
    Maybe.map2 viewState model.windowSize model.state
        |> Maybe.withDefault (H.text "")


viewNextPair : RenderProps -> Pair -> H.Html msg
viewNextPair renderProps pair =
    H.div
        [ HA.class "d-Drops__Widget"
        , HA.class "d-Drops__NextPairWidget"
        ]
        [ H.h2 [ HA.class "d-Drops__WidgetHeader" ]
            [ H.text "Next"
            ]
        , H.div [ HA.class "d-Drops__NextPair" ]
            [ Board.cellsOfPair pair
                |> List.map (viewUnpositionedCell renderProps)
                |> H.div []
            ]
        ]


viewUnpositionedCell : RenderProps -> Cell -> H.Html msg
viewUnpositionedCell ({ cellSize } as renderProps) c =
    H.div
        [ HA.style "position" "relative"
        , HA.style "width" <| px cellSize
        , HA.style "height" <| px cellSize
        ]
        [ Cell.view [ HA.style "position" "unset" ] renderProps { c | x = 0, y = 0 }
        , Cell.viewForm [] renderProps { c | x = 0, y = 0 }
        ]


viewPoints : Int -> H.Html msg
viewPoints points =
    H.div
        [ HA.class "d-Drops__Widget"
        , HA.class "d-Drops__PointsWidget"
        ]
        [ H.h2 [ HA.class "d-Drops__WidgetHeader" ]
            [ H.text "Points"
            ]
        , H.div [ HA.class "d-Drops__Points" ]
            [ H.text (String.fromInt points) ]
        ]


viewGooLayers : State -> RenderProps -> H.Html msg
viewGooLayers state renderProps =
    let
        shouldHighlightGroups =
            case state.playState of
                Evaluating { groupAnimationState } ->
                    groupAnimationState /= Nothing

                _ ->
                    False

        (Pile cells) =
            state.pile
    in
    H.div [ HA.class "d-Drops__GooLayers" ]
        (Cell.colors
            |> List.map
                (\color ->
                    List.filter (.color >> (==) color) cells
                        |> List.map
                            (\cell ->
                                let
                                    shouldHighlight =
                                        cell.isPartOfGroup && shouldHighlightGroups

                                    opacity =
                                        if shouldHighlight then
                                            1

                                        else
                                            -- Weaken bonds when in free fall
                                            1 - (cell.y - toFloat (truncate cell.y))
                                in
                                ( Uuid.toString cell.id
                                , Cell.viewCircle
                                    [ HA.style "transform-origin" "50% 50%"
                                    , HA.style "will-change" "transform opacity"
                                    , if shouldHighlight then
                                        HA.style "animation" "0.33s DropsHighlight ease-in-out infinite alternate"

                                      else if cell.isFresh then
                                        -- Animate growing of bonds when cell first lands on pile
                                        HA.style "animation" "0.5s DropsFadeIn ease-in 1 forwards"

                                      else
                                        -- Mature cells without highlight have no animation
                                        HA.style "animation" "none"
                                    , HA.style "transition" <|
                                        if shouldHighlight || opacity == 0 then
                                            "none"

                                        else
                                            "opacity 0.5s"
                                    , HA.style "opacity" (String.fromFloat opacity)
                                    ]
                                    { x = toFloat cell.x * renderProps.cellSize
                                    , y = cell.y * renderProps.cellSize
                                    , radius = renderProps.cellRadius
                                    , color = color
                                    }
                                )
                            )
                        |> HK.node "div"
                            [ HA.class "d-Drops__GooLayer"
                            ]
                )
        )


viewBoard : RenderProps -> State -> H.Html msg
viewBoard renderProps state =
    let
        (Pile pileCells) =
            state.pile

        activeCells =
            case state.playState of
                Playing ( c1, c2 ) ->
                    [ c1, c2 ]

                Paused ( c1, c2 ) ->
                    [ c1, c2 ]

                _ ->
                    []
    in
    H.div
        [ HA.style "position" "relative" ]
        [ H.div
            [ HA.style "position" "absolute"
            , HA.style "top" "0"
            , HA.style "left" "0"
            , HA.style "width" <| String.fromFloat renderProps.boardWidth ++ "px"
            , HA.style "height" <| String.fromFloat renderProps.boardHeight ++ "px"
            , HA.class "d-Drops__Widget"
            ]
            []
        , -- active pair
          activeCells
            |> List.map
                (\cell ->
                    Cell.view
                        [ HA.classList
                            [ ( "d-Drops__PairCell", True )
                            , ( "d-Drops__PairCell--Active", cell.y >= 0 )
                            ]
                        ]
                        renderProps
                        cell
                )
            |> H.div []

        -- gooey layers to animate bonds between adjacent color group members
        , viewGooLayers state renderProps

        -- all cells are already rendered on the gooey layer, but we need to re-render
        -- ungrouped cells, because on the gooey layer there's an opacity transition applied to them,
        -- causing these to temporarily disappear
        , pileCells
            |> List.filter (not << .isPartOfGroup)
            |> List.map (Cell.viewKeyed [] renderProps)
            |> HK.node "div" []

        -- all cell symbols
        , (pileCells ++ (activeCells |> List.filter (\{ y } -> y >= 0)))
            |> List.map
                (\cell ->
                    Cell.viewForm [] renderProps cell
                )
            |> H.div []

        -- explosions
        , state.explosions
            |> Particle.System.viewCustom (viewParticle renderProps)
                (H.div [ HA.style "filter" "url(#goo)", HA.style "position" "absolute", HA.style "top" "0", HA.style "left" "0" ])

        -- the goo filter
        , S.svg
            [ SA.width <| String.fromFloat renderProps.boardWidth
            , SA.height <| String.fromFloat renderProps.boardHeight
            , SA.fontFamily "sans-serif"
            , SA.style "overflow: visible"
            ]
            [ S.defs []
                -- The choice of filter primitives can have a major impact on performance,
                -- with possible variation between browsers and platforms. This filter
                -- combination has been confirmed to be GPU-accelerated and have good
                -- performance on
                --   - Firefox 106 (Windows, GPU WebRender)
                --   - Firefox 105 (macOS, GPU WebRender)
                --   - Edge 106 (Windows)
                --   - Chrome 104 (Linux)
                -- On Linux Firefox may use a software backend for WebRender in which
                -- case performance is slightly worse.
                -- Safari versions up to 15 do not apply the filter at all, the gooey
                -- effect is missing, performance is good.
                [ S.filter [ SA.id "goo" ]
                    [ S.feGaussianBlur
                        [ SA.in_ "SourceGraphic"
                        , SA.stdDeviation <| String.fromFloat renderProps.blur
                        , SA.result "blur"
                        ]
                        []
                    , S.feColorMatrix [ SA.in_ "blur", SA.mode "matrix", SA.values "1 0 0 0 0  0 1 0 0 0  0 0 1 0 0  0 0 0 19 -9", SA.result "goo" ] []
                    ]
                ]
            ]
        ]


viewParticle : RenderProps -> Particle Explosion -> H.Html msg
viewParticle renderProps particle =
    let
        { color, particleScaleFactor } =
            Particle.data particle
    in
    Cell.viewCircle
        [ HA.style "transform-origin" "50% 50%"
        , HA.style "transform" <| scale (Particle.lifetimePercent particle * 0.4 * particleScaleFactor)
        ]
        { x = Particle.leftPixels particle * renderProps.cellSize
        , y = Particle.topPixels particle * renderProps.cellSize
        , radius = renderProps.cellRadius
        , color = color
        }


type alias RenderProps =
    { boardHeight : Float
    , boardWidth : Float
    , cellRadius : Float
    , cellSize : Float
    , boardX : Float
    , boardY : Float
    , blur : Float
    }


getRenderProps : Window.Size -> RenderProps
getRenderProps window =
    let
        marginY =
            30

        boardHeight =
            toFloat window.height - (2 * marginY)

        boardWidth =
            (toFloat Board.columns / toFloat Board.rows) * boardHeight

        cellSize =
            boardHeight / toFloat Board.rows

        cRadius =
            cellSize * 0.5

        boardX =
            toFloat window.width / 2 - boardWidth / 2

        boardY =
            toFloat window.height / 2 - boardHeight / 2
    in
    { boardHeight = boardHeight
    , boardWidth = boardWidth
    , cellRadius = cRadius
    , cellSize = cellSize
    , boardX = boardX
    , boardY = boardY
    , blur = toFloat window.height * 0.008
    }


viewPauseOverlay : H.Html msg
viewPauseOverlay =
    H.div
        [ HA.class "d-Drops__PauseOverlay"
        , HA.class "d-Drops__Overlay"
        ]
        [ H.div [ HA.class "d-Drops__PauseOverlayIcon" ]
            [ H.img
                [ HA.src "icon.svg"
                , HA.class "d-Drops__Icon"
                ]
                []
            , H.h1 [] [ H.text "Drops" ]
            ]
        , H.div []
            [ H.div
                [ HA.class "d-Drops__Introduction"
                ]
                [ H.section [ HA.class "d-Drops__PauseOverlaySection" ]
                    [ H.p []
                        [ H.text "Drops is an implementation of "
                        , H.a
                            [ HA.href "https://www.wikiwand.com/en/Puyo_Puyo"
                            , HA.target "blank_"
                            ]
                            [ H.text "Puyo Puyo" ]
                        , H.text " by "
                        , H.a
                            [ HA.href "https://dividat.com/"
                            , HA.target "blank_"
                            ]
                            [ H.text "Dividat." ]
                        ]
                    ]
                , H.section [ HA.class "d-Drops__PauseOverlaySection" ]
                    [ H.h2 [ HA.class "d-Drops__PauseOverlayHeading" ]
                        [ H.text "Instructions"
                        ]
                    , H.p []
                        [ H.text "Pairs of colored drops fall down from the top of the screen. When a drop lands near a drop of the same color, they connect into a group. Create four or more drops to make them burst and score points."
                        ]
                    , H.p []
                        [ H.text "You make more points when you clear multiple groups by landing a single pair of drops."
                        ]
                    ]
                , H.section [ HA.class "d-Drops__PauseOverlaySection" ]
                    [ H.h2 [ HA.class "d-Drops__PauseOverlayHeading" ]
                        [ H.text "Controls"
                        ]
                    , H.dl [ HA.class "d-Drops__Controls" ] <|
                        ([ ( "Move left", "Left arrow" )
                         , ( "Move right", "Right arrow" )
                         , ( "Accelerate", "Down arrow" )
                         , ( "Spin", "Space" )
                         , ( "Pause", "Enter" )
                         ]
                            |> List.map
                                (\( t, d ) ->
                                    [ H.dt [ HA.class "d-Drops__ControlName" ] [ H.text t ]
                                    , H.dd [ HA.class "d-Drops__ControlKey" ] [ H.text d ]
                                    ]
                                )
                            |> List.concat
                        )
                    ]
                ]
            , H.div [ HA.class "d-Drops__CTA" ]
                [ H.div []
                    [ H.text "Press ENTER to play"
                    ]
                ]
            ]
        ]


viewState : Window.Size -> State -> H.Html Msg
viewState window state =
    let
        renderProps =
            getRenderProps window
    in
    H.div
        [ HA.class "d-Drops"
        ]
        [ H.div [ HA.class "d-Drops__Widgets" ]
            [ viewNextPair renderProps <|
                -- show active pair as next pair as long as it's above playing field,
                -- only switch to next pair, when it enters field
                case state.playState of
                    Playing pair ->
                        if List.all (\{ y } -> y < 0) <| Board.cellsOfPair pair then
                            pair

                        else
                            state.nextPair

                    _ ->
                        state.nextPair
            , viewBoard renderProps state
            , viewPoints state.metrics.points
            ]
        , case state.playState of
            GameOver ->
                H.div
                    [ HA.class "d-Drops__GameoverOverlay"
                    ]
                    [ H.text "Game Over"
                    ]

            _ ->
                H.text ""
        , case state.playState of
            Paused _ ->
                viewPauseOverlay

            _ ->
                H.text ""
        , UI.timer gameDuration state.time
        ]



-- Explosion


type alias Explosion =
    { particleScaleFactor : Float
    , color : Cell.Color
    }


explosion : Cell.Color -> Float -> Random.Generator (Particle Explosion)
explosion color scaleFactor =
    Random.constant color
        |> Random.map (Explosion scaleFactor)
        |> Particle.init
        |> Particle.withDirection (Random.map degrees (Random.float 0 360))
        |> Particle.withSpeed (Random.Float.normal 5 5)
        |> Particle.withLifetime (Random.Float.normal 2 0.5)


burstCells : Float -> List Cell -> Random.Generator (List (Particle Explosion))
burstCells scaleFactor cells =
    cells
        |> List.map
            (\{ x, y, color } ->
                explosion color scaleFactor
                    |> Particle.withLocation
                        (Random.constant
                            { x = toFloat x
                            , y = y
                            }
                        )
                    |> Particle.withGravity 15
                    |> Particle.withDrag
                        (\_ ->
                            { coefficient = 0.05
                            , density = 0.05
                            , area = 1
                            }
                        )
                    |> Random.list 5
            )
        |> Random.Extra.sequence
        |> Random.map List.concat


burstGroups : List Group -> State -> Return Msg State
burstGroups groups state =
    let
        particleSystem : Particle.System.System Explosion
        particleSystem =
            state.explosions
                |> Particle.System.burst
                    (groups
                        |> List.map (\(Group g) -> g)
                        |> List.concat
                        |> burstCells 1
                    )
    in
    { state
        | explosions = particleSystem
        , pile = Board.removeGroups state.pile
    }
        |> Return.singleton


type alias Explosions =
    Particle.System.System Explosion



-- Settings


gameDuration : Time_
gameDuration =
    5 * Time_.minute
