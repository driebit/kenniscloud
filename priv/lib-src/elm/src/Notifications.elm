module Notifications exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Iso8601
import Json.Decode as Decode
import Json.Encode as Encode
import Keyboard
import Markdown
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Task
import Time exposing (Posix)
import Url.Builder
import Util



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 1000 (TimeTick << Util.millisToNow << Time.posixToMillis)
        , keyboardEvents model.notifications
        ]


keyboardEvents : RemoteData e a -> Sub Msg
keyboardEvents notifications =
    case notifications of
        Success _ ->
            Keyboard.downs (onKeyDowns << Keyboard.anyKeyOriginal)

        _ ->
            Sub.none


onKeyDowns : Maybe Keyboard.Key -> Msg
onKeyDowns key =
    case key of
        Just Keyboard.Enter ->
            ToggleNotifications

        _ ->
            NoOp



-- MODEL


type alias Model =
    { notifications : WebData Notifications
    , alert : Bool
    , id : UserId
    , now : Util.Now
    }


type alias Flags =
    { alert : Bool
    , id : UserId
    , now : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { notifications = NotAsked
      , alert = flags.alert
      , id = flags.id
      , now = Util.millisToNow flags.now
      }
    , Cmd.none
    )


type alias Id =
    String


type alias UserId =
    Int


type Notification
    = NewRemarkOrMention NotificationContent
    | NewEvent NotificationContent
    | NewReference NotificationContent
    | NewContribution NotificationContent


type alias NotificationContent =
    { id : Id
    , url : String
    , topic : String
    , actor : String
    , object : NotificationObject
    , published : Posix
    , mentions : List UserId
    }


type alias PartialNotificationContent =
    { id : Id
    , target_id : String
    , actor_id : String
    , object_id : String
    , published : Posix
    , to : List UserId
    }


type alias NotificationObject =
    { url : String
    , object_type : String
    , title : String
    , content : String
    }


type alias Notifications =
    List Notification



-- UPDATE


type Msg
    = NoOp
    | TimeTick Util.Now
    | ToggleNotifications
    | GotNotifications (WebData Notifications)
    | ClearNotifications
    | GotDeleteListResult (WebData String)
    | NotificationsSeenAtReported (WebData String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        TimeTick now ->
            ( { model | now = now }, Cmd.none )

        ToggleNotifications ->
            case model.notifications of
                Success _ ->
                    ( { model | notifications = NotAsked }
                    , Cmd.none
                    )

                _ ->
                    ( { model | notifications = Loading }
                    , Task.attempt
                        (GotNotifications << Result.withDefault (RemoteData.succeed []))
                        (RemoteData.fromTask getNotificationsTask)
                    )

        GotNotifications data ->
            let
                updateSeenAtRequest =
                    RemoteData.Http.post
                        (Url.Builder.absolute
                            [ "api"
                            , "model"
                            , "kc_user"
                            , "post"
                            , "activity_inbox"
                            ]
                            []
                        )
                        NotificationsSeenAtReported
                        Decode.string
                        (notificationsSeenAtReportEncoder (Util.nowToPosix model.now))
            in
            ( { model | notifications = data, alert = False }
            , updateSeenAtRequest
            )

        NotificationsSeenAtReported _ ->
            ( model
            , Cmd.none
            )

        ClearNotifications ->
            ( { model | notifications = Success [] }
            , RemoteData.Http.delete
                (Url.Builder.absolute
                    [ "api"
                    , "model"
                    , "kc_user"
                    , "delete"
                    , "activity_inbox"
                    ]
                    []
                )
                GotDeleteListResult
                Encode.null
            )

        GotDeleteListResult _ ->
            ( { model | notifications = NotAsked }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    let
        notifications =
            case model.notifications of
                Success notifications_ ->
                    viewNotifications model.now model.id notifications_

                _ ->
                    text ""
    in
    div
        [ class "notifications" ]
        [ button
            [ classList
                [ ( "toggle-notifications", True )
                , ( "-alert", model.alert )
                ]
            , onClick ToggleNotifications
            ]
            [ i [ class "icon--notification" ] []
            ]
        , notifications
        ]


viewNotifications : Util.Now -> UserId -> List Notification -> Html Msg
viewNotifications now user notifications =
    let
        notificationsWithFallback =
            case notifications of
                [] ->
                    [ li [] [ text "Er zijn geen notificaties" ] ]

                _ ->
                    List.map (viewNotification now user) notifications
    in
    div
        [ class "notifications__list" ]
        [ h3 [] [ text "Notificaties" ]
        , button
            [ id "remove-notifications"
            , class "notifications__remove"
            , onClick ClearNotifications
            ]
            [ text "Alle notificaties verwijderen" ]
        , ul [] notificationsWithFallback
        , viewCloseTarget
        ]


viewCloseTarget : Html Msg
viewCloseTarget =
    div
        [ class "notifications__closetarget"
        , onClick ToggleNotifications
        ]
        []


viewNotification : Util.Now -> UserId -> Notification -> Html Msg
viewNotification now user notification =
    case notification of
        NewRemarkOrMention content ->
            viewRemarkNotification now user content

        NewEvent content ->
            viewContributionNotification now user content "Nieuwe meetup van "

        NewReference content ->
            viewContributionNotification now user content "Nieuwe bron van "

        NewContribution content ->
            viewContributionNotification now user content "Nieuwe bijdrage van "


viewRemarkNotification : Util.Now -> UserId -> NotificationContent -> Html Msg
viewRemarkNotification now user notification =
    let
        message =
            case List.member user notification.mentions of
                True ->
                    [ text "Je bent genoemd in een "
                    , a [ href notification.url ]
                        [ text "reactie van "
                        , text notification.actor
                        ]
                    ]

                False ->
                    [ text notification.actor
                    , text ": "
                    , a [ href notification.url ] [ Markdown.toHtml [] notification.object.content ]
                    ]
    in
    viewNotificationContent now user notification message


viewContributionNotification : Util.Now -> UserId -> NotificationContent -> String -> Html Msg
viewContributionNotification now user notification label =
    let
        message =
            [ text label
            , text notification.actor
            , text ": "
            , a [ href notification.object.url ] [ Html.text notification.object.title ]
            ]
    in
    viewNotificationContent now user notification message


viewNotificationContent : Util.Now -> UserId -> NotificationContent -> List (Html Msg) -> Html Msg
viewNotificationContent now user notification message =
    li
        []
        [ h4 [] [ Html.text notification.topic ]
        , p [] message

        -- Note: we don't need the timezone here because the exported date string
        -- always has an offset from UTC, which 'Iso8601.decoder' already accounts for
        , Util.formatTime now notification.published Time.utc
        ]



-- TASKS


getNotificationsTask : Task.Task Http.Error Notifications
getNotificationsTask =
    let
        inboxTask =
            getActivityPubTask
                (Url.Builder.absolute
                    [ "activitypub"
                    , "inbox"
                    ]
                    []
                )
                (Decode.maybe
                    (Decode.field "orderedItems" (Decode.list (Decode.field "@id" Decode.string)))
                    |> Decode.map (Maybe.withDefault [])
                )

        notificationsTask inboxIds =
            Task.sequence (List.map getNotificationTask inboxIds)
    in
    inboxTask |> Task.andThen notificationsTask


getNotificationTask : Id -> Task.Task Http.Error Notification
getNotificationTask notificationId =
    let
        partialNotificationTask =
            getActivityPubTask notificationId partialNotificationContentDecoder

        fullNotificationTask partialContent =
            Task.map3
                (\actorContent targetContent objectContent ->
                    let
                        notificationContent =
                            { id = partialContent.id
                            , url = targetContent.url
                            , topic = targetContent.title
                            , actor = actorContent.title
                            , object = objectContent
                            , published = partialContent.published
                            , mentions = partialContent.to
                            }
                    in
                    case objectContent.object_type of
                        "Note" ->
                            NewRemarkOrMention notificationContent

                        "Event" ->
                            NewEvent notificationContent

                        "Page" ->
                            NewReference notificationContent

                        _ ->
                            NewContribution notificationContent
                )
                (getActivityPubTask partialContent.actor_id notificationObjectDecoder)
                (getActivityPubTask partialContent.target_id notificationObjectDecoder)
                (getActivityPubTask partialContent.object_id notificationObjectDecoder)
    in
    partialNotificationTask |> Task.andThen fullNotificationTask


getActivityPubTask : String -> Decode.Decoder a -> Task.Task Http.Error a
getActivityPubTask url decoder =
    Http.task
        { method = "GET"
        , headers = [ Http.header "Accept" "application/activity+json" ]
        , url = url
        , body = Http.emptyBody
        , resolver = expectJson decoder
        , timeout = Nothing
        }


expectJson : Decode.Decoder a -> Http.Resolver Http.Error a
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
                    case Decode.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (Http.BadBody (Decode.errorToString err))



-- DECODERS


partialNotificationContentDecoder : Decode.Decoder PartialNotificationContent
partialNotificationContentDecoder =
    Decode.map6 PartialNotificationContent
        (Decode.field "@id" Decode.string)
        (decodeFirstId "target")
        (decodeFirstId "actor")
        (decodeFirstId "object")
        (decodeFirstValue "published" Iso8601.decoder)
        decodeTo


decodeTo : Decode.Decoder (List Int)
decodeTo =
    Decode.oneOf
        [ Decode.field "to" (Decode.list decodeUserTo)
        , Decode.succeed []
        ]


decodeUserTo : Decode.Decoder Int
decodeUserTo =
    Decode.field "@id" Decode.string
        |> Decode.andThen
            (\fullId ->
                case fullIdToId fullId of
                    Just id ->
                        Decode.succeed id

                    Nothing ->
                        Decode.fail "Invalid ID path"
            )


fullIdToId : String -> Maybe Int
fullIdToId fullId =
    String.split "/" fullId |> List.reverse |> List.head |> Maybe.andThen String.toInt


notificationObjectDecoder : Decode.Decoder NotificationObject
notificationObjectDecoder =
    Decode.map4 NotificationObject
        (Decode.field "@id" Decode.string)
        (Decode.field "@type" Decode.string)
        (decodeFirstValue "name" Decode.string)
        (Decode.oneOf
            [ decodeFirstValue "content" Decode.string
            , Decode.succeed ""
            ]
        )


decodeFirstId field =
    decodeFirstSubField field "@id" Decode.string


decodeFirstValue field decoder =
    decodeFirstSubField field "@value" decoder


decodeFirstSubField field subfield decoder =
    Decode.oneOf
        [ Decode.field field <| Decode.index 0 <| Decode.field subfield decoder
        , Decode.at [ field, subfield ] decoder
        ]


notificationsSeenAtReportEncoder : Posix -> Encode.Value
notificationsSeenAtReportEncoder time =
    Encode.object
        [ ( "seen_at", Encode.string (Iso8601.fromTime time) )
        ]
