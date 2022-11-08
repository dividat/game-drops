module Main exposing (main)

import Browser
import Drops


main : Program () Drops.Model Drops.Msg
main =
    Browser.element
        { init = always Drops.init
        , update = Drops.update
        , subscriptions = Drops.subscriptions
        , view = Drops.view
        }
