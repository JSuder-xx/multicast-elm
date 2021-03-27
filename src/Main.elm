module Main exposing (Model(..), Msg(..), ViewModel, main, modelToViewModel, processDiagramListView, stringListItemView, update, view)

import Browser
import Html exposing (Html, div, li, nav, text, textarea, ul)
import Html.Attributes exposing (id, style, target, value)
import Html.Events exposing (onClick, onInput)
import MessageId
import MulticastCommunication
import MulticastCommunicationParser
import MulticastDiagram
import MulticastDiagramSimulation
import ProcessNumber
import Svg
import Svg.Attributes exposing (cx, cy, d, height, markerEnd, markerHeight, markerStart, markerWidth, orient, r, refX, refY, stroke, strokeWidth, width, x, x1, x2, y, y1, y2)
import TimeStep
import VectorTime


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( InterprocessCommunicationSpecification "", Cmd.none )
        , update = update
        , view = modelToViewModel >> view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- -----------------------------------------------------------------------------------
-- Model and Update
-- -----------------------------------------------------------------------------------


{-| The model is just a interprocess communication specification. Consequently there is only a single message.
-}
type Model
    = InterprocessCommunicationSpecification String


type Msg
    = UpdateInterprocessCommunicationSpecification String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        UpdateInterprocessCommunicationSpecification text ->
            ( InterprocessCommunicationSpecification text, Cmd.none )



-- -----------------------------------------------------------------------------------
-- View Model
-- -----------------------------------------------------------------------------------


type alias ViewModel =
    { interprocessCommunicationSpecification : String
    , processDiagramResult : Result (List String) (List MulticastDiagram.ProcessDiagram)
    }


modelToViewModel : Model -> ViewModel
modelToViewModel (InterprocessCommunicationSpecification interprocessCommunicationSpecification) =
    let
        processDiagramResult =
            MulticastCommunicationParser.parseProcesses interprocessCommunicationSpecification
                |> Result.andThen
                    (\processes ->
                        let
                            processErrors =
                                MulticastCommunication.interprocessErrors processes
                        in
                        if List.isEmpty processErrors then
                            Ok processes

                        else
                            Err processErrors
                    )
                |> Result.map MulticastDiagramSimulation.interprocessCommunicationToDiagram
    in
    { interprocessCommunicationSpecification = interprocessCommunicationSpecification
    , processDiagramResult = processDiagramResult
    }



-- -----------------------------------------------------------------------------------
-- View
-- -----------------------------------------------------------------------------------


stringListItemView : String -> Html Msg
stringListItemView str =
    li [] [ text str ]


dimensions : { operationHeight : Int, processWidth : Int, headerHeight : Int, markerRadius : Int, leftMargin : Int }
dimensions =
    { operationHeight = 100
    , processWidth = 240
    , headerHeight = 40
    , markerRadius = 4
    , leftMargin = 20
    }


operationCenter : ProcessNumber.ProcessNumber -> Int -> ( Int, Int )
operationCenter processNumber operationIndex =
    ( processNumber |> ProcessNumber.value |> (*) dimensions.processWidth |> (+) dimensions.leftMargin
    , dimensions.markerRadius + dimensions.headerHeight + (dimensions.operationHeight * operationIndex)
    )


markerArrowId : String
markerArrowId =
    "arrow"


markerCircleId : String
markerCircleId =
    "circle"


svgText : ( Int, Int ) -> { fillColor : String, decoration : String } -> String -> Svg.Svg msg
svgText ( xPos, yPos ) { fillColor, decoration } text =
    Svg.text_
        [ Svg.Attributes.style ("fill:" ++ fillColor ++ ";text-decoration: " ++ decoration)
        , xPos |> String.fromInt |> x
        , yPos |> String.fromInt |> y
        ]
        [ Svg.text text ]


processDiagramView : Int -> MulticastDiagram.ProcessDiagram -> List (Html Msg)
processDiagramView maxOperations { processNumber, operations, undeliveredMessages } =
    let
        messageStrings =
            List.map (\{ messageId } -> MessageId.value messageId) >> String.join " | "

        ( xPos, _ ) =
            operationCenter processNumber 0

        xPosStr =
            String.fromInt xPos

        height =
            maxOperations |> (*) dimensions.operationHeight |> (+) dimensions.headerHeight

        operationView operationIndex operation =
            let
                ( markerX, markerY ) =
                    operationCenter processNumber operationIndex
            in
            case operation of
                MulticastDiagram.Wait ->
                    Svg.circle
                        [ r "4"
                        , markerX |> String.fromInt |> cx
                        , markerY |> String.fromInt |> cy
                        , stroke "#666"
                        ]
                        []

                MulticastDiagram.BroadcastOfMessage { messageId, vectorTime } ->
                    [ messageId
                        |> MessageId.value
                    , " "
                    , VectorTime.display vectorTime
                    ]
                        |> String.concat
                        |> svgText ( markerX + 10, markerY - 2 )
                            { fillColor = "white", decoration = "none" }

                MulticastDiagram.ReceiptOfMessage { receivedMessage, messagesDelivered } ->
                    let
                        ( sentX, sentY ) =
                            operationCenter receivedMessage.sentByProcess (TimeStep.value receivedMessage.sentAtTime)

                        isOddProcess =
                            receivedMessage.sentByProcess |> ProcessNumber.value |> modBy 2 |> (==) 1
                    in
                    Svg.g []
                        [ Svg.line
                            [ sentX |> String.fromInt |> x1
                            , sentY |> String.fromInt |> y1
                            , markerX |> String.fromInt |> x2
                            , markerY |> String.fromInt |> y2
                            , strokeWidth "1.5"
                            , stroke
                                (if isOddProcess then
                                    "#ff0"

                                 else
                                    "#88f"
                                )
                            , [ "url(#", markerArrowId, ")" ] |> String.concat |> markerEnd
                            , [ "url(#", markerCircleId, ")" ] |> String.concat |> markerStart
                            ]
                            []
                        , case messagesDelivered of
                            [] ->
                                Svg.text_ [] []

                            _ ->
                                Svg.g []
                                    [ svgText ( markerX + 5, markerY + 8 ) { fillColor = "#4f4", decoration = "underline" } "Delivered:"
                                    , messagesDelivered |> messageStrings |> svgText ( markerX + 5, markerY + 24 ) { fillColor = "#4f4", decoration = "none" }
                                    ]
                        ]
    in
    List.concat
        [ -- Process Header
          [ processNumber |> ProcessNumber.label |> (++) "P" |> svgText ( xPos, 16 ) { fillColor = "white", decoration = "none" } ]

        -- Process vertical line
        , [ Svg.line
                [ x1 xPosStr
                , x2 xPosStr
                , dimensions.headerHeight |> String.fromInt |> y1
                , height |> String.fromInt |> y2
                , stroke "white"
                , strokeWidth "2"
                ]
                []
          ]

        -- all of the Broadcast, Receive and NoOp operations for this process
        , operations |> List.indexedMap operationView
        , [ undeliveredMessages |> messageStrings |> svgText ( xPos, height - 16 ) { fillColor = "#f88", decoration = "underline" } ]
        ]


processDiagramListView : List MulticastDiagram.ProcessDiagram -> Html Msg
processDiagramListView processes =
    let
        numProcess =
            processes |> List.length

        maxOperations =
            processes |> List.map (\process -> List.length process.operations) |> List.foldl max 0
    in
    processes
        |> List.concatMap (processDiagramView maxOperations)
        |> List.append
            [ Svg.defs []
                [ Svg.marker [ id markerArrowId, markerWidth "13", markerHeight "13", refX "10", refY "6", orient "auto" ]
                    [ Svg.path [ d "M2,2 L2,11 L10,6 L2,2", Svg.Attributes.style "fill: #ffffff" ] [] ]
                , Svg.marker
                    [ id markerCircleId, markerWidth "8", markerHeight "8", refX "5", refY "5" ]
                    [ Svg.circle [ cx "5", cy "5", r "3", Svg.Attributes.style "stroke: none; fill:#ffffff;" ] [] ]
                ]
            ]
        |> Svg.svg
            [ numProcess |> (*) dimensions.processWidth |> (+) (dimensions.leftMargin * 2) |> String.fromInt |> width
            , (maxOperations * dimensions.operationHeight) + dimensions.headerHeight + (dimensions.markerRadius * 4) |> String.fromInt |> height
            ]


noTopMargin : Html.Attribute msg
noTopMargin =
    style "margin-top" "0"


exampleLink : String -> String -> Html Msg
exampleLink caption example =
    Html.a
        [ example |> UpdateInterprocessCommunicationSpecification |> onClick
        , style "color" "#adf"
        , style "text-decoration" "underline"
        , style "cursor" "pointer"
        , style "margin-right" "16px"
        ]
        [ Html.text caption ]


specificationView : String -> Html Msg
specificationView specification =
    div []
        [ div [ style "margin-bottom" "8px" ]
            [ Html.span [ style "color" "white", style "margin-right" "8px" ] [ text "Examples:" ]
            , exampleLink "One to Two" "b:hello b:goodbye\nr:hello r:goodbye\n"
            , exampleLink "Two to One" "r:hello r:goodbye\nb:hello b:goodbye\n"
            , exampleLink "Back and Forth" "b:bonjour r:hello b:au_revoir r:bye\nb:hello r:bonjour b:bye r:au_revoir\n"
            , exampleLink "Circular Dependency" "r:b b:a\nr:a b:b\nr:a r:b\n"
            , exampleLink "FIFO" "b:hi b:whats_up b:bye\nr:bye r:whats_up r:hi\nr:hi r:whats_up r:bye\nr:whats_up r:hi r:bye\n"
            , exampleLink "Causal" "b:one r:two_after_one\nr:one b:two_after_one\nr:two_after_one r:one\n"
            , exampleLink "Another Causal" "b:a b:b r:c_after_a b:d\nr:b r:a b:c_after_a r:d\nr:d r:c_after_a r:b r:a\n"
            , exampleLink "Concurrent" "b:a b:b r:c r:d\nb:c b:d r:a r:b\nr:d r:c r:b r:a\n"
            ]
        , div
            [ style "display" "flex", style "margin-bottom" "8px" ]
            [ textarea
                [ style "font-family" "monospace"
                , onInput UpdateInterprocessCommunicationSpecification
                , value specification
                , Html.Attributes.rows 4
                , Html.Attributes.cols 60
                ]
                []
            , div
                [ style "margin-left" "12px"
                , style "margin-top" "2px"
                , style "padding" "2px"
                , style "border" "solid 1px #888"
                , style "background-color" "#333"
                , style "color" "#eee"
                , style "font-family" "sans-serif"
                ]
                [ Html.p
                    [ style "color" "#eee", noTopMargin ]
                    [ Html.text "Enter a list of process communication descriptions in the text area on the left."
                    , [ "One process per line. Be sure to add a return after each process."
                      , "Indicate multicast message broadcast with 'b:MessageIdentifier'."
                      , "Indicate message receipt in a process timeline with 'r:MessageIdentifier'."
                      , "Message Identifiers must be unique. Each process must broadcast or receive every message."
                      , "Message Identifiers can consist of any alpha/numeric characters (no spaces, no punctuation)."
                      , "The simulation will simply halt when encountering a circular dependency."
                      , "Click the example hyperlinks to learn more."
                      ]
                        |> List.map stringListItemView
                        |> Html.ul [ style "margin-top" "4px" ]
                    ]
                ]
            ]
        ]


view : ViewModel -> Browser.Document Msg
view viewModel =
    let
        menuLink caption url =
            Html.a
                [ target "_blank"
                , Html.Attributes.href url
                , style "float" "left"
                , style "color" "white"
                , style "text-align" "center"
                , style "padding" "10px 16px"
                , style "text-decoration" "none"
                , style "font-size" "14px"
                ]
                [ text caption ]
    in
    { title = "Multicast Causal Ordering"
    , body =
        [ nav
            [ style "position" "fixed"
            , style "top" "0"
            , style "height" "36px"
            , style "width" "100%"
            , style "background-color" "#333"
            , style "overflow" "hidden"
            , style "font-family" "sans-serif"
            ]
            [ menuLink "My Home Page" "https://jsuder-xx.github.io"
            , menuLink "On GitHub" "https://github.com/JSuder-xx/multicast-elm"
            , menuLink "Vector Clocks (Wikipedia)" "https://en.wikipedia.org/wiki/Vector_clock"
            ]
        , div
            [ style "padding" "8px"
            , style "margin-top" "37px"
            , style "background-color" "black"
            , style "font-family" "sans-serif"
            ]
            [ specificationView viewModel.interprocessCommunicationSpecification
            , Html.hr [] []
            , case viewModel.processDiagramResult of
                Err errors ->
                    errors |> List.map stringListItemView |> ul [ style "color" "#f66" ]

                Ok processDiagramList ->
                    processDiagramListView processDiagramList
            ]
        ]
    }
