module VectorTime exposing (..)

import Array exposing (Array(..))
import Maybe
import ProcessNumber exposing (ProcessNumber(..))


type VectorTime
    = VectorTime (Array Int)


make : Int -> VectorTime
make size =
    VectorTime (Array.initialize size (\_ -> 0))


display : VectorTime -> String
display (VectorTime vector) =
    [ "["
    , vector |> Array.toList |> List.map String.fromInt |> String.join ", "
    , "]"
    ]
        |> String.concat


incrementForProcess : ProcessNumber -> VectorTime -> Maybe VectorTime
incrementForProcess (ProcessNumber processNumber) (VectorTime vectorArray) =
    Array.get processNumber vectorArray
        |> Maybe.map
            (\existing ->
                VectorTime (Array.set processNumber (existing + 1) vectorArray)
            )


deliverMessage : ( VectorTime, ProcessNumber ) -> VectorTime -> Maybe VectorTime
deliverMessage ( VectorTime senderVector, ProcessNumber senderProcessNumber ) (VectorTime receiverVector) =
    let
        getSenderVectorPositionMaybe =
            Array.get senderProcessNumber

        senderVectorTimeMaybe =
            getSenderVectorPositionMaybe senderVector

        isVeryNextMessageReceiverExpectsFromSender =
            Maybe.map2
                (\sender receiver -> sender == (receiver + 1))
                senderVectorTimeMaybe
                (getSenderVectorPositionMaybe receiverVector)
                |> Maybe.withDefault False

        hasReceiverSeenTheSendersPast =
            List.map2 (\sender receiver -> sender <= receiver)
                (senderVector |> Array.set senderProcessNumber 0 |> Array.toList)
                (Array.toList receiverVector)
                |> List.all identity
    in
    case senderVectorTimeMaybe of
        Nothing ->
            Maybe.Nothing

        Just senderTime ->
            if isVeryNextMessageReceiverExpectsFromSender && hasReceiverSeenTheSendersPast then
                Array.set senderProcessNumber senderTime receiverVector |> VectorTime |> Just

            else
                Nothing
