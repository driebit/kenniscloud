module Remark.Editor exposing (Model, Msg, empty, init, update, view)

import Api
import File
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Remark.Mention as Mention
import Task
import Types exposing (Depiction(..), ForTask(..), Mention, MentionSuggestion, PointIndex, RscDepiction)


type alias Model =
    { body : String
    , pointIndex : PointIndex
    , mentions : List Mention
    , candidate : Maybe Mention.Candidate
    , suggestions : List MentionSuggestion
    , busy : Bool
    , depiction : Depiction
    , isCheckboxChecked : Bool
    , forTask : ForTask
    }


empty : Bool -> Model
empty onTask =
    { body = ""
    , pointIndex = 0
    , mentions = []
    , candidate = Nothing
    , suggestions = []
    , busy = False
    , depiction = NoDepiction
    , isCheckboxChecked = False
    , forTask =
        if onTask then
            IsNotTaskSubmission

        else
            NoOngoingTask
    }


init : String -> List Mention -> Depiction -> Bool -> Bool -> Model
init body mentions depiction onTask isTaskSubmission =
    { body = body
    , pointIndex = 0
    , mentions = mentions
    , candidate = Nothing
    , suggestions = []
    , busy = False
    , depiction = depiction
    , isCheckboxChecked =
        -- start with a pre-checked box if the depiction resource already exists
        case depiction of
            NewDepiction _ ->
                False

            ExistingDepiction _ ->
                True

            NoDepiction ->
                False
    , forTask =
        case onTask of
            False ->
                NoOngoingTask

            True ->
                case isTaskSubmission of
                    True ->
                        IsTaskSubmission

                    False ->
                        IsNotTaskSubmission
    }


type Msg
    = GotMentionSuggestions (Result Http.Error (List MentionSuggestion))
    | OnInput String
    | OnPointMove PointIndex
    | AddSuggestion MentionSuggestion
    | FileSelected File.File
    | RemoveDepiction
    | ToggleCheckbox Bool
    | ToggleTaskCheckbox Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotMentionSuggestions (Ok suggestions) ->
            let
                removeEmpty =
                    List.filter (not << String.isEmpty << .userName)
            in
            ( { model | suggestions = removeEmpty suggestions }, Cmd.none )

        GotMentionSuggestions (Err _) ->
            ( model, Cmd.none )

        OnInput input ->
            let
                removeEdited =
                    List.filter ((\a -> String.contains a input) << .userName)

                ( candidate, candidateCmd ) =
                    findCandidate input model.pointIndex

                suggestions =
                    case candidate of
                        Just _ ->
                            model.suggestions

                        Nothing ->
                            []
            in
            ( { model
                | body = input
                , mentions = removeEdited model.mentions
                , candidate = candidate
                , suggestions = suggestions
              }
            , candidateCmd
            )

        OnPointMove index ->
            ( { model | pointIndex = index }, Cmd.none )

        AddSuggestion suggestion ->
            let
                mention =
                    Mention suggestion.userId suggestion.userName ""
            in
            case model.candidate of
                Just candidate ->
                    ( { model
                        | mentions = mention :: model.mentions
                        , candidate = Nothing
                        , body = Mention.insertCandidate mention candidate
                        , suggestions = []
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        FileSelected file ->
            ( { model | depiction = NewDepiction file }, Cmd.none )

        RemoveDepiction ->
            ( { model | depiction = NoDepiction }, Cmd.none )

        ToggleCheckbox isChecked ->
            ( { model | isCheckboxChecked = isChecked }, Cmd.none )

        ToggleTaskCheckbox isChecked ->
            case model.forTask of
                NoOngoingTask ->
                    ( model, Cmd.none )

                _ ->
                    case isChecked of
                        True ->
                            ( { model | forTask = IsTaskSubmission }, Cmd.none )

                        False ->
                            ( { model | forTask = IsNotTaskSubmission }, Cmd.none )


findCandidate : String -> PointIndex -> ( Maybe Mention.Candidate, Cmd Msg )
findCandidate input pointIndex =
    case Mention.findCandidate input pointIndex of
        Just result ->
            ( Just result
            , Task.attempt GotMentionSuggestions <|
                Api.getMentionSuggestions result.candidate
            )

        Nothing ->
            ( Nothing, Cmd.none )


view : Model -> Html Msg
view model =
    section [ class "remark-editor" ]
        [ textarea
            [ onInput OnInput
            , value model.body
            , autofocus True
            , on "keyup" (Decode.map OnPointMove decodePointIndex)
            , on "click" (Decode.map OnPointMove decodePointIndex)
            ]
            []
        , ul [] (List.map viewSuggestion model.suggestions)
        , ul [] (List.map viewSelectedMention model.mentions)
        , ul [ class "remark-editor__upload-image" ] [ viewDepictionSelector model.depiction ]
        , viewDepictionCheckbox model.depiction model.isCheckboxChecked
        , viewForTaskCheckbox model.forTask
        ]


viewSuggestion : MentionSuggestion -> Html Msg
viewSuggestion suggestion =
    button
        [ class "remark-suggestion"
        , onClick (AddSuggestion suggestion)
        ]
        [ text suggestion.userName ]


viewSelectedMention : Mention -> Html Msg
viewSelectedMention mention =
    div [ class "remark-mentioned" ] [ text mention.userName ]


viewDepictionSelector : Depiction -> Html Msg
viewDepictionSelector depiction =
    case depiction of
        NewDepiction file ->
            viewFile file

        ExistingDepiction rscDepiction ->
            viewRscDepiction rscDepiction

        NoDepiction ->
            div []
                [ label [ for "file-upload-button", class "c-file-upload__label" ] [ text "Upload een bestand" ]
                , input
                    [ type_ "file"
                    , id "file-upload-button"
                    , multiple False
                    , on "input" (Decode.map FileSelected fileDecoder)
                    , required False
                    , accept "image/*"
                    , size 10000000
                    ]
                    []
                ]


viewFile : File.File -> Html Msg
viewFile file =
    li []
        [ text (File.name file)
        , button
            [ onClick RemoveDepiction
            , Html.Events.stopPropagationOn "click" (Decode.succeed ( RemoveDepiction, True ))
            , Html.Events.preventDefaultOn "click" (Decode.succeed ( RemoveDepiction, True ))
            ]
            [ text "x" ]
        ]


viewRscDepiction : RscDepiction -> Html Msg
viewRscDepiction depiction =
    li []
        [ img [ class "remark-editor__image", src depiction.depictionUrl ] []
        , text depiction.depictionTitle
        , button
            [ onClick RemoveDepiction
            , Html.Events.stopPropagationOn "click" (Decode.succeed ( RemoveDepiction, True ))
            , Html.Events.preventDefaultOn "click" (Decode.succeed ( RemoveDepiction, True ))
            ]
            [ text "x" ]
        ]


viewDepictionCheckbox : Depiction -> Bool -> Html Msg
viewDepictionCheckbox depiction isCheckboxChecked =
    let
        checkBoxItem =
            label [ class "remark-editor__checkbox", classList [ ( "remark-editor__checkbox--highlighted", not isCheckboxChecked ) ] ]
                [ input
                    [ type_ "checkbox"
                    , checked isCheckboxChecked
                    , onCheck ToggleCheckbox
                    ]
                    []
                , text "Ik bezit de auteursrechten op dit bestand en/of dit bestand is publiek domein."
                ]
    in
    -- only show the checkbox if a depiction has been picked
    case depiction of
        NewDepiction _ ->
            checkBoxItem

        ExistingDepiction _ ->
            checkBoxItem

        NoDepiction ->
            text ""


viewForTaskCheckbox : ForTask -> Html Msg
viewForTaskCheckbox forTask =
    let
        checkBoxItem isCheckboxChecked =
            label [ class "remark-editor__checkbox" ]
                [ input
                    [ type_ "checkbox"
                    , checked isCheckboxChecked
                    , onCheck ToggleTaskCheckbox
                    ]
                    []
                , text "Indienen voor taakvoltooiing."
                ]
    in
    -- only show the checkbox if this is on an ongoing task
    case forTask of
        NoOngoingTask ->
            text ""

        IsTaskSubmission ->
            checkBoxItem True

        IsNotTaskSubmission ->
            checkBoxItem False



-- DECODERS


decodePointIndex : Decode.Decoder PointIndex
decodePointIndex =
    Decode.oneOf
        [ Decode.at [ "srcElement", "selectionStart" ] Decode.int
        , Decode.at [ "target", "selectionStart" ] Decode.int
        ]


fileDecoder : Decode.Decoder File.File
fileDecoder =
    Decode.field "target" (Decode.field "files" (Decode.index 0 File.decoder))
