module MulticastCommunicationParser exposing (parseProcesses)

import Char
import MessageId exposing (MessageId)
import MulticastCommunication exposing (Communication(..), Process)
import Parser
    exposing
        ( (|.)
        , (|=)
        , DeadEnd
        , Parser
        , Problem(..)
        , andThen
        , map
        , oneOf
        , problem
        , succeed
        , token
        , variable
        )
import Parser.Advanced
import ProcessNumber exposing (ProcessNumber(..))
import Set


spaceParser : Parser ()
spaceParser =
    Parser.chompWhile (\c -> c == ' ' || c == '\t' || c == '\u{000D}')


messageIdParser : Parser MessageId
messageIdParser =
    succeed MessageId.make
        |= variable
            { start = Char.isAlphaNum
            , inner = \c -> Char.isAlphaNum c || (c == '_')
            , reserved = Set.fromList []
            }


sendMessageTokenParser : Parser ()
sendMessageTokenParser =
    token "b:"


receiveMessageTokenParser : Parser ()
receiveMessageTokenParser =
    token "r:"


communicationEventParser : Parser Communication
communicationEventParser =
    oneOf
        [ succeed Broadcast |. sendMessageTokenParser |. spaceParser |= messageIdParser
        , succeed Receive |. receiveMessageTokenParser |. spaceParser |= messageIdParser
        ]
        |. spaceParser


returnParser : Parser ()
returnParser =
    Parser.Advanced.chompIf (\ch -> ch == '\n') (Expecting "carriage return")


processParser : Int -> Parser Process
processParser processNumber =
    let
        parseCommunicationEvents : List Communication -> Parser (List Communication)
        parseCommunicationEvents acc =
            oneOf
                [ communicationEventParser
                    -- I should be using sequence here to avoid the recursion. This would break for really long processes.
                    -- Since this is a toy I don't think it matters.
                    |> andThen (\event -> parseCommunicationEvents (event :: acc))
                , returnParser |. spaceParser |> map (\_ -> acc |> List.reverse)
                ]
    in
    parseCommunicationEvents []
        |> map (\events -> { processNumber = ProcessNumber processNumber, events = events })


processListParser : Parser (List Process)
processListParser =
    let
        helper : List Process -> Parser (List Process)
        helper processesSoFar =
            oneOf
                [ processParser (processesSoFar |> List.length)
                    |> andThen
                        (\process ->
                            helper (process :: processesSoFar)
                        )
                , succeed (processesSoFar |> List.reverse) |. Parser.end
                ]
    in
    helper []


problemToString : Problem -> String
problemToString problem =
    case problem of
        Expecting str ->
            "Expecting '" ++ str ++ "'"

        ExpectingVariable ->
            "Expecting a variable"

        ExpectingEnd ->
            "Expecting end of input."

        Problem str ->
            "Problem: '" ++ str ++ "'"

        _ ->
            "?"


parseProcesses : String -> Result (List String) (List Process)
parseProcesses =
    let
        deadEndToString : DeadEnd -> String
        deadEndToString { col, row, problem } =
            [ "Line: "
            , String.fromInt row
            , ", Col: "
            , String.fromInt col
            , ": "
            , problemToString problem
            ]
                |> String.concat
    in
    Parser.run processListParser >> Result.mapError (List.map deadEndToString)
