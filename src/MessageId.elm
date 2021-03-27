module MessageId exposing (MessageId, make, value)

{-| Identifier for a message that will be broadcast or received.
-}


type MessageId
    = MessageId String


make : String -> MessageId
make id =
    MessageId id


value : MessageId -> String
value (MessageId id) =
    id
