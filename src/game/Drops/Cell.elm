module Drops.Cell exposing
    ( Cell
    , Circle
    , Color(..)
    , colorToCss
    , colors
    , view
    , viewCircle
    , viewForm
    , viewKeyed
    )

import Color
import Drops.CssUtils exposing (px, translate)
import Html as H
import Html.Attributes as HA
import Svg as S
import Svg.Attributes as SA
import Uuid exposing (Uuid)


type Color
    = Black
    | Blue
    | Orange
    | Green
    | Yellow


type alias Cell =
    { color : Color
    , x : Int
    , y : Float
    , fallTo : Maybe Int -- when it has space below and needs to fall
    , isPartOfGroup : Bool
    , isFresh : Bool
    , id : Uuid
    }


colors : List Color
colors =
    [ Black
    , Blue
    , Orange
    , Green
    , Yellow
    ]


{-| The color scheme here is designed to work with various types of color
blindness: protanopia, deuteranopia, tritanopia and achromatopsia.

The palette was obtained using <https://davidmathlogic.com/colorblind/>:

  - starting with the Wong palette,
  - keeping the 5 more distinguishable colors for any type of color blindness,
  - adjusting the remaining colors to improve differentiation (including achromatopsia).

The additional shapes rendered inside the cells help people with black and
white vision (achromatopsia) differentiating the cells, but this is also
beneficial for anyone else.

-}
toColor : Color -> Color.Color
toColor c =
    let
        fromRgba255 r g b =
            Color.fromRgba { red = r / 255, green = g / 255, blue = b / 255, alpha = 1.0 }
    in
    case c of
        Black ->
            fromRgba255 17 17 17

        Blue ->
            fromRgba255 1 71 111

        Orange ->
            fromRgba255 167 75 0

        Green ->
            fromRgba255 93 179 179

        Yellow ->
            fromRgba255 224 210 0


colorToCss : Color -> String
colorToCss =
    toColor >> Color.toCssString


viewForm : List (S.Attribute msg) -> { a | cellSize : Float, cellRadius : Float } -> Cell -> H.Html msg
viewForm attrs { cellSize, cellRadius } cell =
    let
        form =
            case cell.color of
                Black ->
                    S.g
                        []
                        [ S.line
                            [ SA.x1 <| String.fromFloat <| cellSize * 0.35
                            , SA.y1 <| String.fromFloat <| cellSize * 0.65
                            , SA.x2 <| String.fromFloat <| cellSize * 0.65
                            , SA.y2 <| String.fromFloat <| cellSize * 0.35
                            , SA.stroke "white"
                            , SA.strokeLinecap "round"
                            , SA.strokeWidth (cellSize / 8 |> String.fromFloat)
                            ]
                            []
                        , S.line
                            [ SA.x1 <| String.fromFloat <| cellSize * 0.65
                            , SA.y1 <| String.fromFloat <| cellSize * 0.65
                            , SA.x2 <| String.fromFloat <| cellSize * 0.35
                            , SA.y2 <| String.fromFloat <| cellSize * 0.35
                            , SA.stroke "white"
                            , SA.strokeLinecap "round"
                            , SA.strokeWidth (cellSize / 8 |> String.fromFloat)
                            ]
                            []
                        ]

                Blue ->
                    S.rect
                        [ SA.x (String.fromFloat <| cellSize * 0.35)
                        , SA.y (String.fromFloat <| cellSize * 0.35)
                        , SA.width (String.fromFloat <| cellSize * 0.3)
                        , SA.height (String.fromFloat <| cellSize * 0.3)
                        , SA.fill "white"
                        ]
                        []

                Orange ->
                    let
                        scr =
                            cellRadius * 0.2

                        gap =
                            scr * 1.5
                    in
                    S.g
                        []
                        [ S.circle
                            [ SA.cx (String.fromFloat <| cellRadius - (gap / 2) - scr / 2)
                            , SA.cy (String.fromFloat <| cellRadius)
                            , SA.r (String.fromFloat <| scr)
                            , SA.fill "white"
                            ]
                            []
                        , S.circle
                            [ SA.cx (String.fromFloat <| cellRadius + (gap / 2) + scr / 2)
                            , SA.cy (String.fromFloat <| cellRadius)
                            , SA.r (String.fromFloat <| scr)
                            , SA.fill "white"
                            ]
                            []
                        ]

                Green ->
                    S.polygon
                        [ [ ( cellSize * 0.3, cellSize * 0.65 )
                          , ( cellSize * 0.7, cellSize * 0.65 )
                          , ( cellSize * 0.5, cellSize * 0.3 )
                          ]
                            |> List.map (\( x, y ) -> String.fromFloat x ++ " " ++ String.fromFloat y)
                            |> String.join ","
                            |> SA.points
                        , SA.fill "white"
                        ]
                        []

                Yellow ->
                    S.circle
                        [ SA.cx (String.fromFloat <| cellRadius)
                        , SA.cy (String.fromFloat <| cellRadius)
                        , SA.r (String.fromFloat <| cellRadius / 3)
                        , SA.fill "white"
                        ]
                        []
    in
    S.svg
        ([ SA.width (String.fromFloat cellSize)
         , SA.height (String.fromFloat cellSize)
         , HA.style "position" "absolute"
         , HA.style "top" "0"
         , HA.style "left" "0"
         , HA.style "transform" <| translate (toFloat cell.x * cellSize) (cell.y * cellSize)
         , HA.style "will-change" "transform"
         ]
            ++ attrs
        )
        [ form
        ]


view : List (H.Attribute msg) -> { a | cellRadius : Float } -> Cell -> H.Html msg
view attrs { cellRadius } cell =
    viewCircle attrs
        { x = toFloat cell.x * (cellRadius * 2)
        , y = cell.y * (cellRadius * 2)
        , radius = cellRadius
        , color = cell.color
        }


viewKeyed : List (H.Attribute msg) -> { a | cellSize : Float, cellRadius : Float } -> Cell -> ( String, H.Html msg )
viewKeyed attrs renderProps cell =
    ( cell.id |> Uuid.toString, view attrs renderProps cell )


type alias Circle =
    { x : Float
    , y : Float
    , radius : Float
    , color : Color
    }


viewCircle : List (H.Attribute msg) -> Circle -> H.Html msg
viewCircle attrs { x, y, radius, color } =
    H.div
        [ HA.style "transform" <| translate x y
        , HA.style "width" <| px (radius * 2)
        , HA.style "height" <| px (radius * 2)
        , HA.style "top" "0"
        , HA.style "left" "0"
        , HA.style "position" "absolute"
        , HA.style "will-change" "transform"
        ]
        [ H.div
            ([ HA.style "will-change" "transform"
             , HA.style "width" <| px (radius * 2)
             , HA.style "height" <| px (radius * 2)
             , HA.style "border-radius" <| px (radius * 2)
             , HA.style "background-color" (colorToCss color)
             ]
                ++ attrs
            )
            []
        ]
