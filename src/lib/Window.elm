module Window exposing (Size, resizes, size)

{-| Provides a uniform interface for getting the viewport size.

Whereas this functionality is available in Elm 0.19, the API from Elm 0.18 was
more consistent.

<https://package.elm-lang.org/packages/elm-lang/window/1.0.1/Window>

NOTE: This module was partly introduced to ease migration. Its usefulness should
be up for review after more experience using the 0.19 libraries.

-}

import Browser.Dom
import Browser.Events
import Task exposing (Task)


{-| The size of the window in pixels.
-}
type alias Size =
    { width : Int
    , height : Int
    }


{-| Get the current window size.
-}
size : Task x Size
size =
    Browser.Dom.getViewport
        |> Task.map
            (\{ viewport } ->
                { width = truncate viewport.width, height = truncate viewport.height }
            )


{-| Subscribe to any changes in window size.
-}
resizes : (Size -> msg) -> Sub msg
resizes tagger =
    Browser.Events.onResize (\w h -> tagger { width = w, height = h })
