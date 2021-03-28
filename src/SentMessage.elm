module SentMessage exposing (SentMessage, show)

import MessageId
import ProcessNumber
import TimeStep
import VectorTime


type alias SentMessage =
    { messageId : MessageId.MessageId
    , vectorTime : VectorTime.VectorTime
    , sentByProcess : ProcessNumber.ProcessNumber
    , sentAtTime : TimeStep.TimeStep
    }


show : SentMessage -> String
show { messageId, vectorTime, sentByProcess } =
    [ MessageId.value messageId
    , " "
    , VectorTime.display vectorTime
    , ": Sent by Process #"
    , ProcessNumber.label sentByProcess
    ]
        |> String.concat
