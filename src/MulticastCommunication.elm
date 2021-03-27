module MulticastCommunication exposing (Communication(..), Process, interprocessErrors)

import Dict
import List
import Maybe exposing (Maybe(..))
import MessageId exposing (MessageId)
import ProcessNumber exposing (ProcessNumber(..))
import Set exposing (Set(..))


{-| A discrete moment when a process either

  - Broadcasts a specific message (sends)
  - Receives a message that was broadcast by another process.

-}
type Communication
    = Broadcast MessageId
    | Receive MessageId


{-| A single process in a group of processes that are communicating.
-}
type alias Process =
    { processNumber : ProcessNumber, events : List Communication }


processNumberLabel : ProcessNumber -> String
processNumberLabel =
    ProcessNumber.value >> (+) 1 >> String.fromInt


duplicatedBroadcasts : List Process -> List String
duplicatedBroadcasts =
    List.foldl
        (\{ processNumber, events } processAccumulator ->
            events
                |> List.foldl
                    (\event ( broadcastMap, issues ) ->
                        case event of
                            Receive _ ->
                                ( broadcastMap, issues )

                            Broadcast messageId ->
                                let
                                    id =
                                        MessageId.value messageId
                                in
                                case Dict.get id broadcastMap of
                                    Nothing ->
                                        ( Dict.insert id (ProcessNumber.value processNumber) broadcastMap, issues )

                                    Just processWhereFirstSeen ->
                                        ( broadcastMap
                                        , ([ "Process #"
                                           , processNumber |> processNumberLabel
                                           , ": Duplicates broadcast of '"
                                           , id
                                           , "' (first broadcast by process #"
                                           , String.fromInt processWhereFirstSeen
                                           , "). Messages must be unique."
                                           ]
                                            |> String.concat
                                          )
                                            :: issues
                                        )
                    )
                    processAccumulator
        )
        ( Dict.empty, [] )
        >> Tuple.second


discoverProcessMissingHandlingOfMessages : List Process -> List String
discoverProcessMissingHandlingOfMessages processes =
    let
        messagesReceivedByProcess : Process -> Set String
        messagesReceivedByProcess { events } =
            events
                |> List.foldl
                    (\event set ->
                        case event of
                            Receive messageId ->
                                Set.insert (MessageId.value messageId) set

                            Broadcast _ ->
                                set
                    )
                    Set.empty

        messagesBroadcastByProcess : Process -> Set String
        messagesBroadcastByProcess { events } =
            events
                |> List.foldl
                    (\event set ->
                        case event of
                            Receive _ ->
                                set

                            Broadcast messageId ->
                                Set.insert (MessageId.value messageId) set
                    )
                    Set.empty

        messagesHandledByProcess : Process -> Set String
        messagesHandledByProcess process =
            Set.union (messagesReceivedByProcess process) (messagesBroadcastByProcess process)

        allBroadCastMessages =
            processes |> List.foldl (\process set -> Set.union set (messagesBroadcastByProcess process)) Set.empty

        reportProcessIssues process errorDetail messages =
            if List.isEmpty messages then
                []

            else
                [ [ "Process #"
                  , process.processNumber |> processNumberLabel
                  , ": "
                  , errorDetail
                  , ": "
                  , messages |> String.join ","
                  ]
                    |> String.concat
                ]
    in
    processes
        |> List.foldl
            (\process issues ->
                let
                    handled =
                        messagesHandledByProcess process

                    messagesBroadcastAndReceived =
                        Set.intersect (messagesBroadcastByProcess process) (messagesReceivedByProcess process)
                in
                List.concat
                    [ issues
                    , Set.diff allBroadCastMessages handled
                        |> Set.toList
                        |> reportProcessIssues process "Missing receive of messages"
                    , Set.diff handled allBroadCastMessages
                        |> Set.toList
                        |> reportProcessIssues process "Receiving messages not broadcast"
                    , if Set.isEmpty messagesBroadcastAndReceived then
                        []

                      else
                        messagesBroadcastAndReceived |> Set.toList |> reportProcessIssues process "Receiving own broadcasts (you cannot receive a message you send)"
                    ]
            )
            []


{-| Find validation errors in the communication between processes.
-}
interprocessErrors : List Process -> List String
interprocessErrors processes =
    List.concat
        [ duplicatedBroadcasts processes
        , discoverProcessMissingHandlingOfMessages processes
        ]
        |> List.sort
