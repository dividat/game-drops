module Time_ exposing
    ( Time_
    , millisecond, second, minute
    )

{-| Library for working with durations.


# Time\_

@docs Time_, every


# Time\_ units

Units of time, making it easier to specify things like a half-second
`(500 * millisecond)` without remembering Elm’s underlying units of time.

@docs millisecond, second, minute

-}


{-| Type alias to make it clearer when you are working with durations.

Using the `Time_` helpers like `second` instead of raw numbers
is very highly recommended.

-}
type alias Time_ =
    Float



-- Units


millisecond : Time_
millisecond =
    1


second : Time_
second =
    1000 * millisecond


minute : Time_
minute =
    60 * second
