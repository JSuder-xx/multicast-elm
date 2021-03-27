module ProcessNumber exposing (..)


type ProcessNumber
    = ProcessNumber Int



-- | Zero based process number.


value : ProcessNumber -> Int
value (ProcessNumber num) =
    num


label : ProcessNumber -> String
label (ProcessNumber num) =
    String.fromInt <| num + 1
