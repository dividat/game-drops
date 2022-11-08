port module WebAudio exposing (loadSound, loadSounds, playSound, playSounds)

-- PORTS


port loadSound : String -> Cmd msg


port playSound : String -> Cmd msg


loadSounds : List String -> Cmd msg
loadSounds =
    List.map loadSound >> Cmd.batch


playSounds : List String -> Cmd msg
playSounds =
    List.map playSound >> Cmd.batch
