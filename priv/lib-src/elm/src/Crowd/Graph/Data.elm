module Crowd.Graph.Data exposing (NodeObject(..))


type NodeObject
    = Person
        { name : String
        , avatar : Maybe String
        }
    | Tag { name : String }
