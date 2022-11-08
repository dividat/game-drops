module UI exposing (timer)

import Html as H exposing (Html)
import Html.Attributes as HA
import Svg as S
import Svg.Attributes as SA
import Time_ exposing (Time_)


timer : Time_ -> Time_ -> Html msg
timer total now =
    let
        progress =
            (now / total)
                * 100
    in
    H.div [ HA.class "d-GameTimer" ]
        [ H.div
            [ HA.class "d-LinearProgress"
            ]
            [ S.svg [ SA.viewBox "0 0 100 10", SA.preserveAspectRatio "none", SA.width "100%", SA.height "100%" ]
                [ S.rect
                    [ SA.width (String.fromFloat progress)
                    , SA.height "10"
                    , SA.x "0"
                    , SA.y "0"
                    , SA.fill "currentColor"
                    ]
                    []
                ]
            ]
        ]
