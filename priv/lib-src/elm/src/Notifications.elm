module Notifications exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Iso8601
import Json.Decode as Decode
import Json.Encode as Encode
import Keyboard
import Markdown
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
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
    , timezone : Time.Zone
    , mentions : List UserId
    }


type alias NotificationObject =
    { url : String
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
                    , RemoteData.Http.get
                        (Url.Builder.absolute
                            [ "api"
                            , "model"
                            , "driebit_activity2"
                            , "get"
                            , "activities"
                            , "inbox"
                            ]
                            []
                        )
                        GotNotifications
                        notificationsDecoder
                    )

        GotNotifications data ->
            let
                updateSeenAtRequest =
                    RemoteData.Http.post
                        (Url.Builder.absolute
                            [ "api"
                            , "model"
                            , "driebit_activity2"
                            , "post"
                            , "activities"
                            , "inbox"
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
                    , "driebit_activity2"
                    , "delete"
                    , "activities"
                    , "inbox"
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
        , Util.formatTime now notification.published notification.timezone
        ]



-- DECODERS


dateDecoder : Decode.Decoder Posix
dateDecoder =
    Iso8601.decoder


notificationDecoder : Decode.Decoder Notification
notificationDecoder =
    Decode.at [ "object", "type" ] Decode.string
        |> Decode.andThen
            (\value ->
                case value of
                    "remark" ->
                        Decode.map NewRemarkOrMention notificationContentDecoder

                    "event" ->
                        Decode.map NewEvent notificationContentDecoder

                    "reference" ->
                        Decode.map NewReference notificationContentDecoder

                    _ ->
                        Decode.map NewContribution notificationContentDecoder
            )


notificationContentDecoder : Decode.Decoder NotificationContent
notificationContentDecoder =
    Decode.map8 NotificationContent
        (Decode.field "id" Decode.string)
        (Decode.at [ "target", "id" ] Decode.string)
        (Decode.at [ "target", "name" ] Decode.string)
        (Decode.at [ "actor", "name" ] Decode.string)
        (Decode.field "object" notificationObjectDecoder)
        (Decode.field "published" dateDecoder)
        (Decode.field "timezone_offset" Util.decodeTimezone)
        (Decode.field "to" decodeTo)


notificationObjectDecoder : Decode.Decoder NotificationObject
notificationObjectDecoder =
    Decode.map3 NotificationObject
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "content" Decode.string)


decodeTo : Decode.Decoder (List Int)
decodeTo =
    Decode.oneOf
        [ Decode.list Decode.int
        , Decode.succeed []
        ]


notificationsDecoder : Decode.Decoder (List Notification)
notificationsDecoder =
    Decode.field "result" (Decode.field "items" (Decode.list notificationDecoder))


notificationsSeenAtReportEncoder : Posix -> Encode.Value
notificationsSeenAtReportEncoder time =
    Encode.object
        [ ( "seen_at", Encode.string (Iso8601.fromTime time) )
        ]
