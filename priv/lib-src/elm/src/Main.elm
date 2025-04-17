port module Main exposing (main)

import Api
import Browser
import Ginger.Util
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Icon exposing (Icon)
import Process
import Remark.Editor as Editor
import Remark.Viewer
import Task exposing (Task)
import Time
import Types
    exposing
        ( Depiction(..)
        , Group(..)
        , Like
        , Mention
        , PageId
        , ParentId
        , Profile
        , Remark
        , RemarkId
        , Remarks(..)
        , RemarksData
        , RequestDelete
        , Role(..)
        , User(..)
        , UserId
        )
import Util


port scrollIdIntoView : Int -> Cmd msg


port logError : String -> Cmd msg



-- MAIN


main : Program ( PageId, Int, Maybe Int ) Model Msg
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
    Time.every 1000 (TimeTick << Util.millisToNow << Time.posixToMillis)



-- MODEL


type alias Model =
    { remarks : List Remark
    , group : Group
    , user : User
    , editor : Editor
    , pageId : PageId
    , isOngoingTask : Bool
    , now : Util.Now
    , remarkId : Maybe PageId
    }


type Editor
    = Open EditorMode Editor.Model
    | Closed


type EditorMode
    = New ParentId
    | Edit RemarkId RequestDelete


type alias IsReply =
    Bool



-- INIT


init : ( PageId, Int, Maybe Int ) -> ( Model, Cmd Msg )
init ( pageId, now, remarkId ) =
    ( { remarks = []
      , group = DefaultGroup
      , user = Anonymous
      , editor = Closed
      , pageId = pageId
      , isOngoingTask = False
      , now = Util.millisToNow now
      , remarkId = remarkId
      }
    , Task.attempt GotRemarksData (Api.getRemarksData pageId)
    )



-- UPDATE


type Msg
    = TimeTick Util.Now
    | GotRemarksData (Result Http.Error RemarksData)
    | EditorMsg Editor.Msg
    | NewRemark Int
    | EditRemark Int String (List Mention) Depiction Bool
    | PostRemark
    | QuitEditor
    | RequestDeleteRemark
    | ConfirmDeleteRemark
    | LikeRemark RemarkId
    | UnlikeRemark RemarkId
    | FlagRemark RemarkId
    | UnflagRemark RemarkId
    | ScrollToRemark (Maybe RemarkId)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeTick now ->
            ( { model | now = now }, Cmd.none )

        GotRemarksData (Ok { remarks, user, group, ongoing_task }) ->
            ( { model | remarks = remarks, remarkId = Nothing, editor = Closed, user = user, group = group, isOngoingTask = ongoing_task }
            , Task.perform (always (ScrollToRemark model.remarkId)) <| Process.sleep 500
            )

        GotRemarksData (Err e) ->
            ( model, logError <| Util.errorToString e )

        EditorMsg editorMsg ->
            let
                ( newEditor, cmds ) =
                    case model.editor of
                        Open (New id) editor ->
                            Editor.update editorMsg editor
                                |> Tuple.mapFirst (Open (New id))

                        Open (Edit id requestDelete) editor ->
                            Editor.update editorMsg editor
                                |> Tuple.mapFirst (Open (Edit id requestDelete))

                        Closed ->
                            ( Closed, Cmd.none )
            in
            ( { model | editor = newEditor }, Cmd.map EditorMsg cmds )

        NewRemark id ->
            ( { model | editor = Open (New id) (Editor.empty model.isOngoingTask) }
            , Cmd.none
            )

        EditRemark id body mentions depiction isTaskSubmission ->
            ( { model | editor = Open (Edit id False) (Editor.init body mentions depiction model.isOngoingTask isTaskSubmission) }
            , Cmd.none
            )

        QuitEditor ->
            ( { model | editor = Closed }, Cmd.none )

        PostRemark ->
            let
                ( editor, post ) =
                    case model.editor of
                        Open (New id) ({ body, mentions, depiction } as e) ->
                            ( Open (New id) { e | busy = True }
                            , Api.addRemark model.group id body mentions depiction e.forTask
                                |> postAndRefresh model.pageId
                            )

                        Open (Edit id r) ({ body, mentions, depiction } as e) ->
                            ( Open (Edit id r) { e | busy = True }
                            , Api.updateRemark id body mentions depiction e.forTask
                                |> postAndRefresh model.pageId
                            )

                        Closed ->
                            ( Closed, Cmd.none )
            in
            ( { model | editor = editor }, post )

        RequestDeleteRemark ->
            case model.editor of
                Open (Edit id _) editor ->
                    ( { model | editor = Open (Edit id True) editor }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ConfirmDeleteRemark ->
            case model.editor of
                Open (Edit id _) _ ->
                    ( model
                    , postAndRefresh model.pageId (Api.deleteRemark id)
                    )

                _ ->
                    ( { model | editor = Closed }, Cmd.none )

        LikeRemark id ->
            ( model
            , postAndRefresh model.pageId (Api.likeRemark id)
            )

        UnlikeRemark id ->
            ( model
            , postAndRefresh model.pageId (Api.unlikeRemark id)
            )

        FlagRemark id ->
            ( model
            , postAndRefresh model.pageId (Api.flagRemark id)
            )

        UnflagRemark id ->
            ( model
            , postAndRefresh model.pageId (Api.unflagRemark id)
            )

        ScrollToRemark maybeRemarkId ->
            ( model
            , Maybe.withDefault Cmd.none <| Maybe.map scrollIdIntoView maybeRemarkId
            )


postAndRefresh : PageId -> Task Http.Error a -> Cmd Msg
postAndRefresh pageId task =
    task
        |> Task.andThen (always (Api.getRemarksData pageId))
        |> Task.attempt GotRemarksData



-- VIEW


view : Model -> Html Msg
view model =
    section []
        [ viewRemarks model.now model.remarks model.editor model.user False
        , viewParticipate model.editor model.user model.pageId
        ]


viewRemarks : Util.Now -> List Remark -> Editor -> User -> IsReply -> Html Msg
viewRemarks now remarks editor user isReply =
    Ginger.Util.viewIfNot (List.isEmpty remarks) <|
        \_ ->
            section []
                [ ul [ class "remarks-list" ] <|
                    List.map (viewRemark now editor user isReply) <|
                        List.filter .isPublished remarks
                ]


viewRemark : Util.Now -> Editor -> User -> IsReply -> Remark -> Html Msg
viewRemark now editor user isReply remark =
    let
        ( viewBody, viewReplyEditor ) =
            case editor of
                Open (New id) editor_ ->
                    if id == remark.id then
                        ( viewRemarkBody remark, viewNewReply editor_ user )

                    else
                        ( viewRemarkBody remark, text "" )

                Open (Edit id requestDelete) editor_ ->
                    if id == remark.id then
                        ( viewEditRemark editor_ requestDelete, text "" )

                    else
                        ( viewRemarkBody remark, text "" )

                Closed ->
                    ( viewRemarkBody remark, text "" )
    in
    li
        [ class "remark-item__wrapper", id (String.fromInt remark.id) ]
        [ viewRemarkAvatar remark.author
        , div [ class "remark-item" ]
            [ viewBody
            , viewRemarkFooter now remark user isReply
            , viewReplyEditor
            , div [ class "remark-list__replies" ]
                [ viewRemarks now (Types.remarksToList remark.replies) editor user True ]
            ]
        ]



-- REMARK PARTS


viewRemarkAvatar : Profile -> Html Msg
viewRemarkAvatar profile =
    let
        roleClass =
            case List.head <| profile.roles of
                Nothing ->
                    ""

                Just role ->
                    case role of
                        CommunityLibrarian ->
                            "person--cl"

                        ProjectLeader ->
                            "person--pl"

                        Manager ->
                            "person--manager"

                        Specialist _ ->
                            "person--specialist"

                        Member ->
                            ""
    in
    a [ href profile.userUrl, class "person-author", class roleClass ]
        [ header [ class "avatar__wrapper" ]
            [ img [ class "avatar__image", src profile.userAvatarUrl ] [] ]
        ]


viewRemarkBody : Remark -> Html Msg
viewRemarkBody remark =
    let
        remarkImage =
            case remark.depiction of
                ExistingDepiction rscDepiction ->
                    img [ src rscDepiction.depictionUrl ] []

                _ ->
                    text ""

        body =
            if remark.isPublished then
                [ a
                    [ href remark.author.userUrl
                    , class "remark-item__username"
                    ]
                    [ text remark.author.userName ]
                , Remark.Viewer.render remark.body remark.mentions
                , remarkImage
                ]

            else
                [ text "[VERWIJDERD]" ]
    in
    div [ class "remark-item__body" ] body


viewRemarkFooter : Util.Now -> Remark -> User -> IsReply -> Html Msg
viewRemarkFooter now remark user isReply =
    let
        viewFooterActions =
            case user of
                User profile ->
                    viewRemarkFooterSignedIn remark profile.userId isReply

                Anonymous ->
                    text ""
    in
    footer [ class "remark-item__footer" ]
        [ div [ class "remark-item__footer__meta" ]
            [ viewTaskSubmissionIcon remark
            , Util.formatTime now remark.date remark.timezone
            , viewRemarkFooterLikes remark.likes
            ]
        , viewFooterActions
        ]


viewRemarkFooterSignedIn : Remark -> UserId -> IsReply -> Html Msg
viewRemarkFooterSignedIn remark userId isReply =
    let
        likeButton =
            Ginger.Util.viewIf remark.isPublished <|
                \_ ->
                    case List.filter (\like -> like.userId == userId) remark.likes of
                        _ :: _ ->
                            viewFooterButton (UnlikeRemark remark.id) "action--active" Icon.Like ""

                        [] ->
                            viewFooterButton (LikeRemark remark.id) "action" Icon.Like "Waarderen"

        flagButton =
            Ginger.Util.viewIf remark.isPublished <|
                \_ ->
                    case List.filter (\flag -> flag.userId == userId) remark.flags of
                        _ :: _ ->
                            viewFooterButton (UnflagRemark remark.id)
                                "flag--active"
                                Icon.Flag
                                "Aangemeld als ongepast - melding intrekken"

                        [] ->
                            viewFooterButton
                                (FlagRemark remark.id)
                                "flag"
                                Icon.Flag
                                "Meld deze bijdrage als ongepast"
    in
    div [ class "remark-item__footer__actions" ]
        [ Ginger.Util.viewIf (userId == remark.author.userId) <|
            \_ ->
                viewFooterButton (EditRemark remark.id remark.body remark.mentions remark.depiction remark.for_task) "action" Icon.Pen "Bewerken"
        , Ginger.Util.viewIfNot isReply <|
            \_ ->
                viewFooterButton (NewRemark remark.id) "respond" Icon.Remarks "Reageren"
        , likeButton
        , flagButton
        ]


viewTaskSubmissionIcon : Remark -> Html Msg
viewTaskSubmissionIcon remark =
    case remark.for_task of
        False ->
            text ""

        True ->
            img [ class "list-item-kg__status-icon", src "/lib/images/icon-done.svg" ] []


viewFooterButton : msg -> String -> Icon -> String -> Html msg
viewFooterButton msg className icon title =
    button [ onClick msg, class ("btn--remark-" ++ className) ]
        [ Icon.view icon
        , span [] [ text title ]
        ]



-- NEW AND EDIT REMARKS


viewNewReply : Editor.Model -> User -> Html Msg
viewNewReply editor user =
    case user of
        Anonymous ->
            text "Log in of registreer om een reactie te geven"

        User profile ->
            viewNewRemarkEditor editor profile


viewNewRemarkEditor : Editor.Model -> Profile -> Html Msg
viewNewRemarkEditor editor profile =
    div [ class "new-remark" ]
        [ viewRemarkAvatar profile
        , div [ class "reply-add" ]
            [ Html.map EditorMsg (Editor.view editor)
            , footer [ class "remark-edit__footer" ]
                [ button
                    [ onClick QuitEditor
                    ]
                    [ text "Annuleer" ]
                , button
                    [ onClick PostRemark
                    , disabled editor.busy
                    ]
                    [ text "Voeg reactie toe" ]
                ]
            ]
        ]


viewEditRemark : Editor.Model -> RequestDelete -> Html Msg
viewEditRemark editor requestDelete =
    div [ class "remark-edit" ]
        [ Html.map EditorMsg (Editor.view editor)
        , viewEditRemarkFooter (canSubmit editor) requestDelete
        ]


canSubmit : Editor.Model -> Bool
canSubmit editor =
    let
        -- only require the checkbox to be checked if a depiction has been picked
        checkboxMissing =
            case editor.depiction of
                NewDepiction _ ->
                    not editor.isCheckboxChecked

                ExistingDepiction _ ->
                    not editor.isCheckboxChecked

                NoDepiction ->
                    False
    in
    not editor.busy && not checkboxMissing


viewEditRemarkFooter : Bool -> RequestDelete -> Html Msg
viewEditRemarkFooter submitEnabled requestDelete =
    let
        deleteButton =
            if requestDelete then
                button
                    [ onClick ConfirmDeleteRemark
                    , class "btn--remark-action"
                    ]
                    [ text "Weet je het zeker?" ]

            else
                button
                    [ onClick RequestDeleteRemark
                    , class "btn--remark-action"
                    ]
                    [ text "Verwijderen" ]
    in
    footer [ class "remark-edit__footer" ]
        [ deleteButton
        , button [ onClick QuitEditor ] [ text "Annuleren" ]
        , button [ onClick PostRemark, disabled (not submitEnabled) ] [ text "Sla wijzigingen op" ]
        ]



-- HTML HELPERS


viewRemarkFooterLikes : List Like -> Html msg
viewRemarkFooterLikes likes =
    Ginger.Util.viewIfNot (List.isEmpty likes) <|
        \_ ->
            let
                body =
                    case likes of
                        [ _ ] ->
                            "1 waardering"

                        xs ->
                            String.fromInt (List.length xs) ++ " waarderingen"
            in
            span [ class "remark-item__footer__likes" ]
                [ text body ]



-- CALL TO ACIONS


viewParticipate : Editor -> User -> PageId -> Html Msg
viewParticipate editor user pageId =
    case ( user, editor ) of
        ( Anonymous, _ ) ->
            text ""

        ( User profile, Closed ) ->
            viewParticipateButton profile pageId

        ( User profile, Open (New id) editor_ ) ->
            Ginger.Util.viewIf (id == pageId) <|
                \_ ->
                    viewNewRemarkEditor editor_ profile

        _ ->
            text ""


viewParticipateButton : Profile -> PageId -> Html Msg
viewParticipateButton profile pageId =
    div [ class "status" ]
        [ img [ class "avatar", src profile.userAvatarUrl ]
            []
        , button
            [ class "btn--new-remark", onClick (NewRemark pageId) ]
            [ text "Reageer op deze bijdrage" ]
        ]
