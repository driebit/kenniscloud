module Api exposing
    ( addReferenceUrl
    , addRemark
    , deleteAbouts
    , deleteRemark
    , flagRemark
    , getMentionSuggestions
    , getRemarksData
    , likeRemark
    , unflagRemark
    , unlikeRemark
    , updateRemark
    )

import Http
import Iso8601
import Json.Decode as D
import Json.Decode.Pipeline as P
import Json.Encode as E
import Task exposing (Task)
import Time exposing (Posix)
import Types
    exposing
        ( Depiction(..)
        , Flag
        , ForTask(..)
        , Group(..)
        , Like
        , Mention
        , MentionSuggestion
        , PageId
        , PageIdOrRemarkId
        , Profile
        , Remark
        , RemarkId
        , Remarks(..)
        , RemarksData
        , Role(..)
        , RscDepiction
        , User(..)
        )
import Util


getRemarksData : PageId -> Task Http.Error RemarksData
getRemarksData id =
    getTask ("/api/model/remarks/get/for/" ++ String.fromInt id) decodeRemarksData


getMentionSuggestions : String -> Task Http.Error (List MentionSuggestion)
getMentionSuggestions query =
    let
        url =
            "/api/model/suggestion/get?cat=person&text=" ++ query
    in
    getTask url (D.list decodeMentionSuggestion)


addRemark : Group -> PageIdOrRemarkId -> String -> List Mention -> Depiction -> ForTask -> Task Http.Error ()
addRemark group parentId body mentions depiction forTask =
    let
        groupProp =
            case group of
                DefaultGroup ->
                    []

                Group g ->
                    [ Http.stringPart "content_group_id" (String.fromInt g) ]

        -- new remarks can only be added with a new depiction from file
        depictionProp =
            case depiction of
                NewDepiction file ->
                    [ Http.filePart "filename[]" file ]

                ExistingDepiction _ ->
                    []

                NoDepiction ->
                    []

        forTaskProp =
            case forTask of
                NoOngoingTask ->
                    []

                IsTaskSubmission ->
                    [ Http.stringPart "for_task" "true" ]

                IsNotTaskSubmission ->
                    [ Http.stringPart "for_task" "false" ]

        postBody =
            [ Http.stringPart "body" body
            , Http.stringPart "title" (String.left 25 body)
            , Http.stringPart "about" (String.fromInt parentId)
            , Http.stringPart "mentions" (String.join ", " (List.map (\m -> String.fromInt m.userId) mentions))
            ]
                ++ groupProp
                ++ depictionProp
                ++ forTaskProp
    in
    Http.task
        { method = "POST"
        , headers = []
        , url = "/api/model/remarks/post/new/"
        , body = Http.multipartBody postBody
        , resolver = expectNoContent
        , timeout = Nothing
        }


deleteAbouts : PageIdOrRemarkId -> Task Http.Error ()
deleteAbouts subjectId =
    postTaskNoContent "delete_abouts" subjectId E.null


addReferenceUrl : PageIdOrRemarkId -> String -> Task Http.Error ()
addReferenceUrl parentId url =
    postTaskNoContent "reference" parentId (E.string url)


updateRemark : RemarkId -> String -> List Mention -> Depiction -> ForTask -> Task Http.Error ()
updateRemark id body mentions depiction forTask =
    let
        -- remarks can be updated with a new depiction, an existing one or none
        depictionProp =
            case depiction of
                -- in the case of a new depiction we send the file
                NewDepiction file ->
                    [ Http.filePart "filename[]" file ]

                -- if using the existing depiction, we send no update
                ExistingDepiction _ ->
                    []

                -- if using no depiction, we update with an empty list
                NoDepiction ->
                    [ Http.stringPart "filename[]" "" ]

        forTaskProp =
            case forTask of
                NoOngoingTask ->
                    []

                IsTaskSubmission ->
                    [ Http.stringPart "for_task" "true" ]

                IsNotTaskSubmission ->
                    [ Http.stringPart "for_task" "false" ]

        postBody =
            [ Http.stringPart "id" (String.fromInt id)
            , Http.stringPart "body" body
            , Http.stringPart "mentions" (String.join ", " (List.map (\m -> String.fromInt m.userId) mentions))
            ]
                ++ depictionProp
                ++ forTaskProp
    in
    Http.task
        { method = "POST"
        , headers = []
        , url = "/api/model/remarks/post/update/" ++ String.fromInt id
        , body = Http.multipartBody postBody
        , resolver = expectNoContent
        , timeout = Nothing
        }


deleteRemark : RemarkId -> Task Http.Error ()
deleteRemark id =
    postTaskNoContent "delete" id E.null


likeRemark : RemarkId -> Task Http.Error ()
likeRemark id =
    postTaskNoContent "like" id E.null


unlikeRemark : RemarkId -> Task Http.Error ()
unlikeRemark id =
    postTaskNoContent "unlike" id E.null


flagRemark : RemarkId -> Task Http.Error ()
flagRemark id =
    postTaskNoContent "flag" id E.null


unflagRemark : RemarkId -> Task Http.Error ()
unflagRemark id =
    postTaskNoContent "unflag" id E.null


decodeRemarksData : D.Decoder RemarksData
decodeRemarksData =
    D.succeed RemarksData
        |> P.optional "remarks" (D.list decodeRemark) []
        |> P.optional "user" decodeUserStatus Anonymous
        |> P.optional "group" decodeGroup DefaultGroup
        |> P.optional "ongoing_task" D.bool False


decodeGroup : D.Decoder Group
decodeGroup =
    D.int
        |> D.andThen (D.succeed << Group)


decodeRemark : D.Decoder Remark
decodeRemark =
    D.lazy
        (\_ ->
            D.succeed Remark
                |> P.required "id" D.int
                |> P.optional "is_published" D.bool False
                |> P.optional "body" D.string ""
                |> P.required "date" decodeDate
                |> P.required "timezone_offset" Util.decodeTimezone
                |> P.optional "likes" (D.list decodeLike) []
                |> P.optional "flags" (D.list decodeFlag) []
                |> P.optional "replies" decodeReplies (Remarks [])
                |> P.optional "mentions" (D.list decodeMention) []
                |> P.optional "depiction" decodeDepiction NoDepiction
                |> P.optional "for_task" D.bool False
                |> P.required "author" decodeProfile
        )


decodeReplies : D.Decoder Remarks
decodeReplies =
    D.map Remarks (D.list (D.lazy (\_ -> decodeRemark)))


decodeMention : D.Decoder Mention
decodeMention =
    D.succeed Mention
        |> P.required "user_id" D.int
        |> P.optional "username" D.string ""
        |> P.optional "user_uri" D.string ""


decodeDepiction : D.Decoder Depiction
decodeDepiction =
    D.succeed RscDepiction
        |> P.required "depiction_id" D.int
        |> P.optional "depiction_title" D.string ""
        |> P.optional "depiction_url" D.string ""
        |> D.map ExistingDepiction


decodeDate : D.Decoder Posix
decodeDate =
    Iso8601.decoder



-- LIKES


decodeLike : D.Decoder Like
decodeLike =
    D.succeed Like
        |> P.required "edge_id" D.int
        |> P.required "user_id" D.int
        |> P.optional "username" D.string ""



-- FLAGS


decodeFlag : D.Decoder Flag
decodeFlag =
    D.succeed Flag
        |> P.required "edge_id" D.int
        |> P.required "user_id" D.int
        |> P.optional "username" D.string ""



-- USER


decodeUserStatus : D.Decoder User
decodeUserStatus =
    D.field "user_signed_in" D.bool
        |> D.andThen decodeUserStatusHelper


decodeUserStatusHelper : Bool -> D.Decoder User
decodeUserStatusHelper isSignedIn =
    if not isSignedIn then
        D.succeed Anonymous

    else
        decodeUser


decodeUser : D.Decoder User
decodeUser =
    D.map User decodeProfile


decodeProfile : D.Decoder Profile
decodeProfile =
    D.succeed Profile
        |> P.required "user_id" D.int
        |> P.optional "user_title" D.string ""
        |> P.optional "user_subtitle" D.string ""
        |> P.optional "user_avatar_url" D.string ""
        |> P.optional "user_uri" D.string ""
        |> P.required "user_roles" (D.list decodeRole)


decodeRole : D.Decoder Role
decodeRole =
    D.oneOf
        [ D.map Specialist D.int
        , D.string
            |> D.andThen
                (\string ->
                    case string of
                        "community_librarian" ->
                            D.succeed CommunityLibrarian

                        "project_leader" ->
                            D.succeed ProjectLeader

                        "manager" ->
                            D.succeed Manager

                        "member" ->
                            D.succeed Member

                        _ ->
                            D.fail ("Unrecognized role: " ++ string)
                )
        ]



-- MENTION SUGGESTION


decodeTranslatedText : D.Decoder String
decodeTranslatedText =
    D.oneOf
        [ D.at [ "tr", "nl" ] D.string
        , D.at [ "tr", "en" ] D.string
        , D.string
        ]


decodeMentionSuggestion : D.Decoder MentionSuggestion
decodeMentionSuggestion =
    D.succeed MentionSuggestion
        |> P.required "id" D.int
        |> P.optional "title" decodeTranslatedText ""



-- HELPERS


expectJson : D.Decoder a -> Http.Resolver Http.Error a
expectJson decoder =
    Http.stringResolver <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata _ ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ _ body ->
                    case D.decodeString (D.field "result" decoder) body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (Http.BadBody (D.errorToString err))


expectNoContent : Http.Resolver Http.Error ()
expectNoContent =
    Http.stringResolver <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata _ ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ _ _ ->
                    Ok ()


getTask : String -> D.Decoder a -> Task Http.Error a
getTask url decoder =
    Http.task
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , resolver = expectJson decoder
        , timeout = Nothing
        }


postTaskNoContent : String -> RemarkId -> E.Value -> Task Http.Error ()
postTaskNoContent action id body =
    Http.task
        { method = "POST"
        , headers = []
        , url = "/api/model/remarks/post/" ++ action ++ "/" ++ String.fromInt id
        , body = Http.jsonBody body
        , resolver = expectNoContent
        , timeout = Nothing
        }
