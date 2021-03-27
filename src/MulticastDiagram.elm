module MulticastDiagram exposing (ProcessDiagram, ProcessOperation(..), ReceiptInformation, SentMessage, deliverMessage, deliverMessages)

import Array exposing (Array(..))
import MessageId exposing (MessageId)
import ProcessNumber exposing (ProcessNumber)
import TimeStep
import VectorTime exposing (VectorTime)


type alias SentMessage =
    { messageId : MessageId
    , vectorTime : VectorTime.VectorTime
    , sentByProcess : ProcessNumber
    , sentAtTime : TimeStep.TimeStep
    }


type alias ReceiptInformation =
    { receivedMessage : SentMessage
    , messagesDelivered : List SentMessage
    }


{-| One of three different operations that a process can perform at a timestep of the simulation
-}
type ProcessOperation
    = Wait
    | BroadcastOfMessage SentMessage
    | ReceiptOfMessage ReceiptInformation


type alias ProcessDiagram =
    { processNumber : ProcessNumber
    , operations : List ProcessOperation
    , undeliveredMessages : List SentMessage
    }


deliverMessage : SentMessage -> VectorTime.VectorTime -> Maybe VectorTime.VectorTime
deliverMessage sentMessage receiverVector =
    VectorTime.deliverMessage ( sentMessage.vectorTime, sentMessage.sentByProcess ) receiverVector


scanForDeliverable : List SentMessage -> VectorTime -> ( List SentMessage, List SentMessage, VectorTime )
scanForDeliverable undeliveredMessages clock =
    undeliveredMessages
        |> List.foldl
            (\sentMessage ( delivered, undelivered, currentClock ) ->
                case deliverMessage sentMessage currentClock of
                    Nothing ->
                        ( delivered
                        , sentMessage :: undelivered
                        , currentClock
                        )

                    Just newClock ->
                        ( sentMessage :: delivered
                        , undelivered
                        , newClock
                        )
            )
            ( [], [], clock )


deliverMessages : List SentMessage -> VectorTime -> { currentClock : VectorTime, undelivered : List SentMessage, delivered : List SentMessage }
deliverMessages sentMessages startingClock =
    let
        whileUncoveringNewlyDeliverable accumulatedDelivered undeliveredMessages clock =
            let
                ( resultDelivered, resultUndelivered, resultClock ) =
                    scanForDeliverable undeliveredMessages clock
            in
            if List.isEmpty resultDelivered then
                ( accumulatedDelivered, resultUndelivered, resultClock )

            else
                whileUncoveringNewlyDeliverable (List.append resultDelivered accumulatedDelivered) resultUndelivered resultClock

        ( finalDelivered, finalUndelivered, finalClock ) =
            whileUncoveringNewlyDeliverable [] sentMessages startingClock
    in
    { currentClock = finalClock
    , undelivered = finalUndelivered
    , delivered = finalDelivered |> List.reverse
    }
