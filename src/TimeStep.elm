module TimeStep exposing (TimeStep, increment, start, value)


type TimeStep
    = TimeStep Int


increment : TimeStep -> TimeStep
increment (TimeStep step) =
    TimeStep (step + 1)


start : TimeStep
start =
    TimeStep 0


value : TimeStep -> Int
value (TimeStep v) =
    v
