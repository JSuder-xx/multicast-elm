module MulticastDiagramSimulation exposing (interprocessCommunicationToDiagram)

import Dict exposing (Dict(..))
import MessageId
import MulticastCommunication
import MulticastDiagram exposing (ProcessDiagram, ProcessOperation(..), SentMessage, deliverMessage, deliverMessages)
import ProcessNumber exposing (ProcessNumber)
import TimeStep
import VectorTime exposing (VectorTime)


type alias ProcessDiagramInterim =
    { processNumber : ProcessNumber
    , currentClock : VectorTime
    , communicationsToProcess : List MulticastCommunication.Communication
    , undeliveredMessages : List SentMessage
    , operations : List ProcessOperation
    }


initializeProcessDiagramInterim : VectorTime -> MulticastCommunication.Process -> ProcessDiagramInterim
initializeProcessDiagramInterim initialClock process =
    { processNumber = process.processNumber
    , currentClock = initialClock
    , communicationsToProcess = process.events
    , undeliveredMessages = []
    , operations = []
    }


interimToProcessDiagram : ProcessDiagramInterim -> ProcessDiagram
interimToProcessDiagram { processNumber, operations, undeliveredMessages } =
    { processNumber = processNumber, operations = operations |> List.reverse, undeliveredMessages = undeliveredMessages }


incrementClock : ProcessDiagramInterim -> ProcessDiagramInterim
incrementClock process =
    case VectorTime.incrementForProcess process.processNumber process.currentClock of
        Nothing ->
            -- narsty, swallowing error
            process

        Just newClock ->
            { process | currentClock = newClock }


addOperation : ProcessOperation -> ProcessDiagramInterim -> ProcessDiagramInterim
addOperation operation process =
    { process | operations = operation :: process.operations }


setCommunicationsToProcess : List MulticastCommunication.Communication -> ProcessDiagramInterim -> ProcessDiagramInterim
setCommunicationsToProcess communications process =
    { process | communicationsToProcess = communications }


addUndeliveredMessage : SentMessage -> ProcessDiagramInterim -> ProcessDiagramInterim
addUndeliveredMessage sentMessage process =
    { process | undeliveredMessages = sentMessage :: process.undeliveredMessages }


type alias MessageDict =
    Dict String SentMessage


simulateStep : TimeStep.TimeStep -> MessageDict -> ProcessDiagramInterim -> ( ProcessDiagramInterim, Maybe SentMessage, Bool )
simulateStep timeStep messagesBroadcastFromPrior process =
    case process.communicationsToProcess of
        [] ->
            ( process, Nothing, False )

        communication :: remainingCommunications ->
            case communication of
                MulticastCommunication.Receive messageId ->
                    case Dict.get (MessageId.value messageId) messagesBroadcastFromPrior of
                        Nothing ->
                            -- waiting because the message we need is not available
                            ( process |> addOperation Wait, Nothing, False )

                        Just sentMessage ->
                            case deliverMessage sentMessage process.currentClock of
                                Nothing ->
                                    -- unable to deliver so adding to the queue
                                    ( process
                                        |> setCommunicationsToProcess remainingCommunications
                                        |> addOperation (ReceiptOfMessage { receivedMessage = sentMessage, messagesDelivered = [] })
                                        |> addUndeliveredMessage sentMessage
                                    , Nothing
                                    , True
                                    )

                                Just myClockAfterDelivery ->
                                    -- we were able to deliver at least this message, so now let us
                                    -- see if we can deliver more messages
                                    let
                                        { currentClock, delivered, undelivered } =
                                            deliverMessages
                                                process.undeliveredMessages
                                                myClockAfterDelivery
                                    in
                                    ( { process
                                        | currentClock = currentClock
                                        , undeliveredMessages = undelivered
                                        , communicationsToProcess = remainingCommunications
                                      }
                                        |> addOperation
                                            (ReceiptOfMessage
                                                { receivedMessage = sentMessage
                                                , messagesDelivered = sentMessage :: delivered
                                                }
                                            )
                                    , Nothing
                                    , True
                                    )

                MulticastCommunication.Broadcast messageId ->
                    let
                        newProcess =
                            process
                                |> incrementClock
                                |> setCommunicationsToProcess remainingCommunications

                        message : SentMessage
                        message =
                            { messageId = messageId
                            , vectorTime = newProcess.currentClock
                            , sentByProcess = process.processNumber
                            , sentAtTime = timeStep
                            }
                    in
                    ( newProcess
                        |> addOperation (BroadcastOfMessage message)
                    , Just message
                    , True
                    )


simulateStepForProcesses : TimeStep.TimeStep -> MessageDict -> List ProcessDiagramInterim -> ( List ProcessDiagramInterim, MessageDict, Bool )
simulateStepForProcesses timeStep messagesBroadcastFromPrior processes =
    processes
        |> List.foldl
            (\process ( processed, messagesBroadcast, progress ) ->
                let
                    ( newProcess, sentMessageMaybe, progressFromProcess ) =
                        simulateStep timeStep messagesBroadcastFromPrior process

                    newMessagesBroadcast =
                        case sentMessageMaybe of
                            Nothing ->
                                messagesBroadcast

                            Just sentMessage ->
                                Dict.insert (sentMessage.messageId |> MessageId.value) sentMessage messagesBroadcast
                in
                ( newProcess :: processed, newMessagesBroadcast, progress || progressFromProcess )
            )
            ( [], messagesBroadcastFromPrior, False )


interprocessCommunicationToDiagram : List MulticastCommunication.Process -> List ProcessDiagram
interprocessCommunicationToDiagram multicastCommunicationProcesses =
    let
        simulateUntilNoProgress timeStep messagesBroadcastSoFar interimProcesses =
            let
                ( nextProcesses, nextMessagesBroadcast, anyProgress ) =
                    simulateStepForProcesses timeStep messagesBroadcastSoFar interimProcesses
            in
            if not anyProgress then
                nextProcesses

            else
                simulateUntilNoProgress (TimeStep.increment timeStep) nextMessagesBroadcast nextProcesses
    in
    multicastCommunicationProcesses
        |> List.map (initializeProcessDiagramInterim (multicastCommunicationProcesses |> List.length |> VectorTime.make))
        |> simulateUntilNoProgress TimeStep.start Dict.empty
        |> List.map interimToProcessDiagram
