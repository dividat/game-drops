module Util exposing (send)

{-| Utilities
-}

import Task


{-| Send a message
-}
send : a -> Cmd a
send msg =
    Task.perform (always msg) (Task.succeed ())
