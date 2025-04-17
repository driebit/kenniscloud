module Edit.Edge exposing
    ( Model
    , Msg
    , entry
    , init
    , itemConnected
    , main
    , requestSuggestions
    , selection
    , selectionCanHaveMore
    , selectionFromResources
    , selectionNeedsMore
    , update
    , validated
    , view
    )

import Browser
import Browser.Dom
import Data.Suggestions as Suggestions exposing (Suggestions)
import Ginger.Id exposing (ResourceId)
import Ginger.Translation as Translation exposing (Language, Translation)
import Ginger.Util exposing (viewIf)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import List.Zipper as Zipper
import Process
import Task
import Url.Builder
import Util


main : Program Decode.Value Model Msg
main =
    Browser.element
        { init = init << decodeFlags
        , update = update
        , view = view Nothing
        , subscriptions = \_ -> Sub.none
        }


decodeFlags : Decode.Value -> Flags
decodeFlags value =
    case Decode.decodeValue flagsDecoder value of
        Ok flags ->
            flags

        Err msg ->
            { defaultFlags
                | error = Just (Decode.errorToString msg)
            }


defaultFlags : Flags
defaultFlags =
    { subject = Nothing
    , predicate = "hasPart"
    , category = "text"
    , label = Nothing
    , placeholder = "Type to search"
    , textInputId = "edit-edge-default-id"
    , minimum = Nothing
    , maximum = Nothing
    , allowNew = Nothing
    , checkboxes = False
    , suggestions = []
    , selection = []
    , focus = False
    , error = Nothing
    , allowExisting = True
    }


flagsDecoder : Decoder Flags
flagsDecoder =
    Decode.succeed Flags
        |> Decode.optional "subject" (Decode.nullable Ginger.Id.fromJson) Nothing
        |> Decode.required "predicate" Decode.string
        |> Decode.required "category" Decode.string
        |> Decode.required "label" (Decode.nullable Decode.string)
        |> Decode.optional "placeholder" Decode.string defaultFlags.placeholder
        |> Decode.required "textInputId" Decode.string
        |> Decode.optional "minimum" (Decode.nullable Decode.int) defaultFlags.minimum
        |> Decode.optional "maximum" (Decode.nullable Decode.int) defaultFlags.maximum
        |> Decode.optional "allowNew" (Decode.nullable Decode.string) defaultFlags.allowNew
        |> Decode.optional "checkboxes" Decode.bool defaultFlags.checkboxes
        |> Decode.optional "suggestions" (Decode.list Suggestions.itemFromJson) []
        |> Decode.optional "selection" (Decode.list Suggestions.itemFromJson) []
        |> Decode.optional "focus" Decode.bool defaultFlags.focus
        |> Decode.optional "error" (Decode.nullable Decode.string) Nothing
        |> Decode.optional "allow_existing" Decode.bool defaultFlags.allowExisting


type alias Model =
    { subject : Maybe ResourceId
    , predicate : String
    , category : String
    , term : Maybe String
    , suggestions : Suggestions
    , label : Maybe String
    , placeholder : String
    , language : Language
    , selection : List ( Bool, Suggestions.Item )
    , selectionMin : Maybe Int
    , selectionMax : Maybe Int
    , selectionAllowsNew : Maybe String
    , selectionCheckboxes : Bool
    , textInputId : String
    , error : Maybe String
    , allowExisting : Bool
    }


type alias Flags =
    { subject : Maybe ResourceId
    , predicate : String
    , category : String
    , label : Maybe String
    , placeholder : String
    , textInputId : String
    , minimum : Maybe Int
    , maximum : Maybe Int
    , allowNew : Maybe String
    , checkboxes : Bool
    , suggestions : List Suggestions.Item
    , selection : List Suggestions.Item
    , focus : Bool
    , error : Maybe String
    , allowExisting : Bool
    }


type Control
    = Button
    | Checkbox Bool


getSelected : List ( Bool, Suggestions.Item ) -> List Suggestions.Item
getSelected =
    List.filterMap
        (\( selected, item ) ->
            if selected then
                Just item

            else
                Nothing
        )


getSuggested : List ( Bool, Suggestions.Item ) -> List Suggestions.Item
getSuggested =
    List.filterMap
        (\( selected, item ) ->
            if selected then
                Nothing

            else
                Just item
        )


setSelected : List Suggestions.Item -> List ( Bool, Suggestions.Item ) -> List ( Bool, Suggestions.Item )
setSelected selected items =
    let
        selected_ =
            List.map (\item -> ( True, item )) selected

        suggested =
            List.map (\item -> ( False, item )) <|
                List.filter (\s -> not <| List.member s selected) <|
                    List.filterMap
                        (\( isSelected, item ) ->
                            if isSelected then
                                Nothing

                            else
                                Just item
                        )
                        items
    in
    selected_ ++ suggested


setSuggested : List Suggestions.Item -> List ( Bool, Suggestions.Item ) -> List ( Bool, Suggestions.Item )
setSuggested suggested items =
    let
        selected =
            List.filter Tuple.first items

        suggested_ =
            List.map (\item -> ( False, item )) <|
                List.filter (\s -> not <| List.member ( True, s ) selected) suggested
    in
    selected ++ suggested_


setSelectedItem : Bool -> Suggestions.Item -> List ( Bool, Suggestions.Item ) -> List ( Bool, Suggestions.Item )
setSelectedItem selected ( itemId, _ ) =
    List.map
        (\( selected_, ( itemId_, value ) ) ->
            if itemId_ == itemId then
                ( selected, ( itemId, value ) )

            else
                ( selected_, ( itemId_, value ) )
        )


itemConnected : Model -> Msg -> Maybe Suggestions.Item
itemConnected model msg =
    case msg of
        Connect item ->
            Just item

        ConnectCurrent ->
            case model.suggestions of
                Suggestions.Empty ->
                    Nothing

                Suggestions.NoFocus _ _ ->
                    Nothing

                Suggestions.HasFocus suggestions ->
                    Just <| Zipper.current suggestions

        _ ->
            Nothing


selectionCanHaveMore : Model -> Bool
selectionCanHaveMore model =
    case ( model.selectionMax, model.selectionAllowsNew ) of
        ( Nothing, _ ) ->
            True

        ( Just max, Nothing ) ->
            max >= List.length (getSelected model.selection)

        ( Just max, _ ) ->
            max > List.length (getSelected model.selection)


selectionNeedsMore : Model -> Bool
selectionNeedsMore model =
    case ( model.selectionMin, model.selectionAllowsNew, model.term ) of
        ( Nothing, _, _ ) ->
            False

        ( Just min, Nothing, _ ) ->
            min > List.length (getSelected model.selection)

        ( Just min, Just _, Nothing ) ->
            min > List.length (getSelected model.selection)

        ( Just min, Just _, Just _ ) ->
            min > 1 + List.length (getSelected model.selection)


selectionFromResources : Language -> List { id : ResourceId, title : Translation } -> List Suggestions.Item
selectionFromResources lang =
    List.map (\{ id, title } -> ( Just id, Translation.toString lang title ))


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        givenSuggestions =
            case ( flags.checkboxes, flags.suggestions ) of
                ( True, _ ) ->
                    Suggestions.Empty

                ( False, [] ) ->
                    Suggestions.Empty

                ( False, first :: rest ) ->
                    Suggestions.NoFocus first rest

        givenSelection =
            if flags.checkboxes then
                setSelected flags.selection []
                    |> setSuggested flags.suggestions

            else
                setSelected flags.selection []
    in
    ( { subject = flags.subject
      , predicate = flags.predicate
      , category = flags.category
      , term = Nothing
      , suggestions = givenSuggestions
      , label = flags.label
      , placeholder = flags.placeholder
      , language = Translation.NL
      , selection = givenSelection
      , selectionMin = flags.minimum
      , selectionMax = flags.maximum
      , selectionAllowsNew = flags.allowNew
      , selectionCheckboxes = flags.checkboxes
      , textInputId = flags.textInputId
      , error = flags.error
      , allowExisting = flags.allowExisting
      }
    , if flags.focus then
        focus flags.textInputId

      else
        Cmd.none
    )


type Msg
    = NoOp
    | OnInput String
    | OnInputFromSuggestions String
    | Escape
    | Backspace
    | SuggestUp
    | SuggestDown
    | FocusOn Suggestions.Item
    | ClearSuggestions
    | ConnectCurrent
    | Connect Suggestions.Item
    | Remove Suggestions.Item
    | RequestSuggestions String
    | GotSuggestions String (Result Http.Error (List Suggestions.Item))
    | PostedEdge ResourceId (Result Http.Error ())
    | DeletedEdge ResourceId (Result Http.Error ())


entry : Model -> Maybe String
entry model =
    model.term


selection : Model -> List Suggestions.Item
selection model =
    getSelected model.selection


validated : Model -> Bool
validated model =
    let
        count =
            List.length (getSelected model.selection)

        lowerBound =
            case model.selectionMin of
                Nothing ->
                    True

                Just bound ->
                    case ( model.selectionAllowsNew, model.term ) of
                        ( Just _, Just _ ) ->
                            (1 + count) >= bound

                        _ ->
                            count >= bound

        upperBound =
            case model.selectionMax of
                Nothing ->
                    True

                Just bound ->
                    count <= bound
    in
    lowerBound && upperBound


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OnInput val ->
            let
                value =
                    String.trimLeft val

                term =
                    emptyIsNothing value
            in
            ( { model | term = term }
            , case ( term, model.allowExisting ) of
                ( Nothing, _ ) ->
                    Cmd.none

                ( _, False ) ->
                    Cmd.none

                ( Just t, True ) ->
                    delayedRequestSuggestions t
            )

        OnInputFromSuggestions key ->
            if String.length key == 1 then
                let
                    term_ =
                        case model.term of
                            Nothing ->
                                key

                            Just t ->
                                t ++ key
                in
                ( { model | term = Just term_ }
                , Cmd.batch
                    [ delayedRequestSuggestions term_
                    , focus model.textInputId
                    ]
                )

            else
                ( model, Cmd.none )

        Escape ->
            ( { model
                | suggestions = Suggestions.Empty
              }
            , focus model.textInputId
            )

        Backspace ->
            case model.term of
                Nothing ->
                    ( model, focus model.textInputId )

                Just term ->
                    let
                        term_ =
                            String.dropRight 1 term
                    in
                    ( { model
                        | term = Just term_
                        , suggestions = Suggestions.Empty
                      }
                    , Cmd.batch
                        [ delayedRequestSuggestions term_
                        , focus model.textInputId
                        ]
                    )

        SuggestUp ->
            let
                suggestions =
                    Suggestions.previous model.suggestions
            in
            ( { model
                | suggestions = suggestions
              }
            , Cmd.none
            )

        SuggestDown ->
            let
                suggestions =
                    Suggestions.next model.suggestions
            in
            ( { model
                | suggestions = suggestions
              }
            , Cmd.none
            )

        FocusOn item ->
            ( { model
                | suggestions =
                    Suggestions.focus item model.suggestions
              }
            , Cmd.none
            )

        ConnectCurrent ->
            case model.suggestions of
                Suggestions.Empty ->
                    ( model, Cmd.none )

                Suggestions.NoFocus first _ ->
                    update (Connect first) model

                Suggestions.HasFocus suggestions ->
                    update (Connect (Zipper.current suggestions)) model

        Connect item ->
            let
                item_ =
                    case item of
                        ( Nothing, _ ) ->
                            ( Nothing, Maybe.withDefault "" model.term )

                        _ ->
                            item
            in
            ( { model
                | selection =
                    if model.selectionCheckboxes then
                        if model.subject == Nothing then
                            setSelectedItem True item_ model.selection

                        else
                            model.selection

                    else
                        model.selection ++ [ ( True, item_ ) ]
                , suggestions = Suggestions.Empty
                , term = Nothing
              }
            , case ( model.selectionCheckboxes, model.subject, item_ ) of
                ( True, Just subject, ( Just object, _ ) ) ->
                    postEdge subject model.predicate object

                ( _, _, _ ) ->
                    focus model.textInputId
            )

        Remove item ->
            ( { model
                | selection =
                    if model.selectionCheckboxes then
                        if model.subject == Nothing then
                            setSelectedItem False item model.selection

                        else
                            model.selection

                    else
                        List.filter ((/=) ( True, item )) model.selection
              }
            , case ( model.selectionCheckboxes, model.subject, item ) of
                ( True, Just subject, ( Just object, _ ) ) ->
                    deleteEdge subject model.predicate object

                ( _, _, _ ) ->
                    focus model.textInputId
            )

        ClearSuggestions ->
            ( { model | suggestions = Suggestions.Empty }, Cmd.none )

        RequestSuggestions term ->
            if (model.term == Just term) && (String.length term > 2) then
                ( model
                , requestSuggestions (GotSuggestions term) model.language model.category term
                )

            else
                ( model, Cmd.none )

        GotSuggestions _ (Ok []) ->
            let
                suggestions =
                    case ( model.term, model.selectionAllowsNew ) of
                        ( Just _, Just labelAddNew ) ->
                            Suggestions.NoFocus ( Nothing, labelAddNew ) []

                        _ ->
                            Suggestions.Empty
            in
            ( { model
                | suggestions = suggestions
                , selection = setSuggested [] model.selection
              }
            , Cmd.none
            )

        GotSuggestions term (Ok suggestions) ->
            if Just term == model.term then
                case List.filter (\s -> not (List.member ( True, s ) model.selection)) suggestions of
                    hd :: rest ->
                        if model.selectionCheckboxes then
                            ( { model | selection = setSuggested (hd :: rest) model.selection }
                            , Cmd.none
                            )

                        else
                            let
                                s =
                                    case ( model.term, model.selectionAllowsNew ) of
                                        ( Nothing, _ ) ->
                                            Suggestions.Empty

                                        ( _, Just labelAddNew ) ->
                                            Suggestions.NoFocus ( Nothing, labelAddNew ) (hd :: rest)

                                        _ ->
                                            Suggestions.NoFocus hd rest
                            in
                            ( { model | suggestions = s }
                            , focus <| viewSuggestionsId model.textInputId
                            )

                    _ ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )

        GotSuggestions _ (Err _) ->
            ( model, Cmd.none )

        PostedEdge rsc (Ok ()) ->
            ( { model
                | selection = setSelectedItem True ( Just rsc, "" ) model.selection
              }
            , Cmd.none
            )

        PostedEdge _ (Err _) ->
            ( { model | error = Just "Post edge error" }, Cmd.none )

        DeletedEdge rsc (Ok ()) ->
            ( { model
                | selection = setSelectedItem False ( Just rsc, "" ) model.selection
              }
            , Cmd.none
            )

        DeletedEdge _ (Err _) ->
            ( { model | error = Just "Delete edge error" }, Cmd.none )


focus : String -> Cmd Msg
focus id =
    Task.attempt (\_ -> NoOp) (Browser.Dom.focus id)


emptyIsNothing : String -> Maybe String
emptyIsNothing val =
    if String.isEmpty val then
        Nothing

    else
        Just val


delayedRequestSuggestions : String -> Cmd Msg
delayedRequestSuggestions t =
    Task.perform (\_ -> RequestSuggestions t) <|
        Process.sleep 300


requestSuggestions :
    (Result Http.Error (List Suggestions.Item) -> msg)
    -> Language
    -> String
    -> String
    -> Cmd msg
requestSuggestions toMsg language category term =
    Http.get
        { url =
            Url.Builder.absolute
                [ "api", "model", "suggestion", "get", "query" ]
                [ Url.Builder.string "cat" category
                , Url.Builder.string "text" term
                ]
        , expect =
            Http.expectJson toMsg (suggestionsFromJson language)
        }


suggestionsFromJson : Language -> Decode.Decoder (List Suggestions.Item)
suggestionsFromJson language =
    Decode.field "result" (Decode.list (decodeSuggestion language))


decodeSuggestion : Language -> Decode.Decoder Suggestions.Item
decodeSuggestion language =
    Decode.map2 Tuple.pair
        (Decode.maybe (Decode.field "id" Ginger.Id.fromJson))
        (Decode.field "title" (Decode.map (Translation.toString language) Util.decodeTranslation))


postEdge : ResourceId -> String -> ResourceId -> Cmd Msg
postEdge subject predicate object =
    let
        url =
            Url.Builder.absolute
                [ "api"
                , "model"
                , "edge"
                , "post"
                , "o"
                , Ginger.Id.toString subject
                , predicate
                , Ginger.Id.toString object
                ]
                []
    in
    Http.post
        { url = url
        , body = Http.jsonBody (Encode.object [])
        , expect = Http.expectWhatever (PostedEdge object)
        }


deleteEdge : ResourceId -> String -> ResourceId -> Cmd Msg
deleteEdge subject predicate object =
    let
        url =
            Url.Builder.absolute
                [ "api"
                , "model"
                , "edge"
                , "delete"
                , "o"
                , Ginger.Id.toString subject
                , predicate
                , Ginger.Id.toString object
                ]
                []
    in
    Http.request
        { method = "DELETE"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectWhatever (DeletedEdge object)
        , timeout = Nothing
        , tracker = Nothing
        }


view : Maybe (String -> String) -> Model -> Html Msg
view newValueFormatter model =
    let
        alwaysStop x =
            ( x, True )

        valid =
            validated model

        classes =
            if valid || model.term == Nothing then
                "form-control"

            else
                "form-control alert-danger"
    in
    div
        [ class "edit-edges"
        , title <|
            case model.error of
                Nothing ->
                    ""

                Just msg ->
                    msg
        ]
        ([ label
            [ for model.textInputId ]
            [ text <| Maybe.withDefault "" model.label ]
         , viewIf
            (not model.selectionCheckboxes
                && (model.selection
                        == []
                        || not valid
                        || (valid && selectionCanHaveMore model)
                   )
            )
            (\_ ->
                input
                    [ onInput OnInput
                    , placeholder model.placeholder
                    , type_ "text"
                    , value (Maybe.withDefault "" model.term)
                    , autocomplete False
                    , id model.textInputId
                    , name (model.textInputId ++ "_entry")
                    , stopPropagationOn "keydown" <|
                        Decode.andThen inputKeyDecoder (Decode.field "key" Decode.string)
                    , class classes
                    ]
                    []
            )
         ]
            ++ (case model.selectionCheckboxes of
                    True ->
                        [ ul [ class "selection selection-checkboxes" ] <|
                            viewCheckboxes model.textInputId model.selection
                        ]

                    False ->
                        [ viewSuggestions model.textInputId model.language model.suggestions
                        , viewIf
                            (model.selection /= [])
                            (\_ ->
                                ul [ class "selection selection-buttons" ] <|
                                    viewSelection model.textInputId newValueFormatter model.selection
                            )
                        ]
               )
        )


unselected : Suggestions -> List Suggestions.Item -> List Suggestions.Item
unselected suggestions selectedItems =
    List.foldl
        (\s acc ->
            if List.member s selectedItems then
                acc

            else
                s :: acc
        )
        []
        (Suggestions.toList suggestions)


inputKeyDecoder : String -> Decode.Decoder ( Msg, Bool )
inputKeyDecoder key =
    case key of
        "Escape" ->
            Decode.succeed ( ClearSuggestions, True )

        "Enter" ->
            Decode.succeed ( ConnectCurrent, True )

        "ArrowUp" ->
            Decode.succeed ( SuggestUp, True )

        "ArrowDown" ->
            Decode.succeed ( SuggestDown, True )

        _ ->
            Decode.fail "Unhandled key"


viewCheckboxes : String -> List ( Bool, Suggestions.Item ) -> List (Html Msg)
viewCheckboxes baseId =
    List.map (\( selected, item ) -> viewSelectionItem (Checkbox selected) baseId Nothing item)


viewSelection : String -> Maybe (String -> String) -> List ( Bool, Suggestions.Item ) -> List (Html Msg)
viewSelection baseId maybeNewValueFormatter =
    List.map (viewSelectionItem Button baseId maybeNewValueFormatter << Tuple.second)


viewSelectionItem : Control -> String -> Maybe (String -> String) -> Suggestions.Item -> Html Msg
viewSelectionItem control baseId maybeNewValueFormatter (( maybeRscId, title ) as item) =
    let
        rscIdStr =
            Maybe.withDefault title <|
                Maybe.map Ginger.Id.toString maybeRscId

        id_ =
            baseId ++ "[" ++ rscIdStr ++ "]"

        connectOrRemove =
            if control == Checkbox False then
                Connect item

            else
                Remove item
    in
    li
        [ class "form-control selected" ]
        [ input
            [ type_ "checkbox"
            , id id_
            , name baseId
            , value rscIdStr
            , checked (control /= Checkbox False)
            , onClick connectOrRemove
            ]
            []
        , case ( maybeRscId, maybeNewValueFormatter ) of
            ( Nothing, Just formatter ) ->
                label [ for id_, class "add-new" ] [ text (formatter title) ]

            _ ->
                label [ for id_ ] [ text title ]
        , viewIf (control == Button)
            (\_ -> i [ class "remove", onClick (Remove item) ] [])
        ]


viewSuggestionsId : String -> String
viewSuggestionsId controlId =
    controlId ++ "-suggestions"


suggestionsKeyDecoder : String -> Decode.Decoder ( Msg, Bool )
suggestionsKeyDecoder key =
    case key of
        "Backspace" ->
            Decode.succeed ( Backspace, True )

        _ ->
            Decode.succeed ( OnInputFromSuggestions key, True )


viewSuggestions : String -> Language -> Suggestions -> Html Msg
viewSuggestions controlId language suggestions =
    let
        id_ =
            id <| viewSuggestionsId controlId

        classes =
            class "form-control suggestions"

        passOnFailToInput key =
            Decode.oneOf
                [ inputKeyDecoder key
                , suggestionsKeyDecoder key
                ]

        wiring =
            stopPropagationOn "keydown" <|
                Decode.andThen
                    passOnFailToInput
                    (Decode.field "key" Decode.string)
    in
    case suggestions of
        Suggestions.Empty ->
            text ""

        Suggestions.NoFocus hd rest ->
            select [ id_, classes, wiring, multiple True ] <|
                List.map (viewSuggestion language False) (hd :: rest)

        Suggestions.HasFocus suggestions_ ->
            select [ id_, classes, wiring, multiple True ] <|
                List.concat
                    [ List.map (viewSuggestion language False) (Zipper.before suggestions_)
                    , [ viewSuggestion language True (Zipper.current suggestions_) ]
                    , List.map (viewSuggestion language False) (Zipper.after suggestions_)
                    ]


viewSuggestion : Language -> Bool -> Suggestions.Item -> Html Msg
viewSuggestion language isSelected (( item, title ) as suggestion) =
    let
        c =
            case item of
                Nothing ->
                    "search-suggestion search-suggestion-new"

                _ ->
                    "search-suggestion"
    in
    option
        [ -- selected isSelected <== this confuses the mechanism currently, need to investigate
          Maybe.map Ginger.Id.toString item
            |> Maybe.withDefault title
            |> value
        , class c
        , onClick <| Connect suggestion
        ]
        [ text title
        ]
