module Crowd.Request exposing
    ( AnonymousUser
    , Crowd
    , Participant(..)
    , Request
    , Tag
    , User
    , crowd
    , getAvatar
    , participantId
    , participantTags
    , run
    , tagsOf
    )

import Crowd.Route as Route
import Ginger.Id as Id exposing (ResourceId)
import Ginger.Media as Media exposing (Media)
import Ginger.Translation as Translation exposing (Translation)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Task
import Url.Builder
import Util
import Zotonic.Internal.Request as Request


type alias Request e a msg =
    (Result e a -> msg) -> Cmd msg


run : Request e a msg -> (Result e a -> msg) -> Cmd msg
run request handle =
    request <| \result -> handle result


map : (a -> b) -> Request e a msg -> Request e b msg
map f request handle =
    request <|
        handle
            << Result.map f


type alias Crowd =
    { title : Translation
    , id : ResourceId
    , participants : List Participant
    , tags : List Tag
    , isElevated : Bool
    }


type Participant
    = Participant User
    | Anonymous AnonymousUser


type alias AnonymousUser =
    { id : ResourceId
    , tags : List Tag
    }


type alias User =
    { id : ResourceId
    , title : Translation
    , tags : List Tag
    , avatar : Maybe String
    , summary : Translation
    , email : Maybe String
    , isAnonymous : Bool
    }


type alias Tag =
    { id : ResourceId
    , title : Translation
    }


crowd : ResourceId -> Maybe Route.CrowdLink -> Request Http.Error Crowd msg
crowd id crowdLink =
    let
        crowdLinkPath =
            Maybe.withDefault "" (Maybe.map Route.unCrowdLink crowdLink)

        url =
            Url.Builder.absolute
                [ "api", "model", "crowd", "get", "data", Id.toString id, crowdLinkPath ]
                []
    in
    \handler ->
        Http.get
            { url = url
            , expect =
                Http.expectJson handler
                    (Decode.field "result"
                        (Decode.succeed Crowd
                            |> Decode.required "title" Util.decodeTranslation
                            |> Decode.hardcoded id
                            |> Decode.required "participants" (Decode.list participantFromJson)
                            |> Decode.required "tags" (Decode.list tagFromJson)
                            |> Decode.optional "is_elevated" Decode.bool False
                        )
                    )
            }


tagsOf : ResourceId -> (Result Http.Error (List Tag) -> msg) -> Cmd msg
tagsOf id msg =
    let
        tagListUrl =
            Url.Builder.absolute
                [ "api"
                , "model"
                , "edge"
                , "get"
                , "o_props"
                , Id.toString id
                , "subject"
                ]
                []

        tagListDecoder =
            Decode.field "result" (Decode.list (Decode.field "object_id" Id.fromJson))

        tagListTask =
            Request.getTask tagListUrl tagListDecoder

        tagTitleUrl tagId =
            Url.Builder.absolute
                [ "api"
                , "model"
                , "rsc"
                , "get"
                , Id.toString tagId
                , "title"
                ]
                []

        tagTitleDecoder tagId =
            Decode.field "result" (Decode.map (Tag tagId) Util.decodeTranslationOrDefault)

        tagTitleTask tagId =
            Request.getTask (tagTitleUrl tagId) (tagTitleDecoder tagId)

        tagTitlesTask taskIds =
            Task.sequence (List.map tagTitleTask taskIds)
    in
    Task.attempt msg (tagListTask |> Task.andThen tagTitlesTask)


participantFromJson : Decode.Decoder Participant
participantFromJson =
    Decode.oneOf
        [ Decode.map Participant userFromJson
        , Decode.map Anonymous anonymousUserFromJson
        ]


userFromJson : Decode.Decoder User
userFromJson =
    Decode.succeed User
        |> Decode.required "id" Id.fromJson
        |> Decode.required "title" Util.decodeTranslation
        |> Decode.required "tags" (Decode.list tagFromJson)
        |> Decode.optional "avatar" (Decode.nullable Decode.string) Nothing
        |> Decode.required "summary" Util.decodeTranslation
        |> Decode.optional "email" (Decode.nullable Decode.string) Nothing
        |> Decode.required "categories"
            (Decode.map (List.member "anonymous_participant") (Decode.list Decode.string))


anonymousUserFromJson : Decode.Decoder AnonymousUser
anonymousUserFromJson =
    Decode.succeed AnonymousUser
        |> Decode.required "id" Id.fromJson
        |> Decode.required "tags" (Decode.list tagFromJson)


tagFromJson : Decode.Decoder Tag
tagFromJson =
    Decode.succeed Tag
        |> Decode.required "id" Id.fromJson
        |> Decode.required "title" Util.decodeTranslation



-- HELPERS


getAvatar : User -> Maybe String
getAvatar user =
    user.avatar


participantTags : Participant -> List Tag
participantTags participant =
    case participant of
        Participant user ->
            user.tags

        Anonymous anonymousUser ->
            anonymousUser.tags


participantId : Participant -> Id.ResourceId
participantId participant =
    case participant of
        Participant user ->
            user.id

        Anonymous anonymousUser ->
            anonymousUser.id
