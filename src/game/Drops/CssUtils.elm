module Drops.CssUtils exposing (px, scale, translate)


px : Float -> String
px n =
    String.fromFloat n ++ "px"


translate : Float -> Float -> String
translate x y =
    "translate(" ++ px x ++ "," ++ px y ++ ")"


scale : Float -> String
scale n =
    "scale(" ++ String.fromFloat n ++ ")"
