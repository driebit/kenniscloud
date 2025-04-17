port module AddEdgeToNewResource exposing (main)

import Browser
import Browser.Dom
import Data.Suggestions as Suggestions exposing (Suggestions)
import Edit.Edge as Edge
import Ginger.Id
import Ginger.Translation as Translation exposing (Language)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import List.Zipper as Zipper
import Process
import Task
import Url.Builder
import Util


port portValues : { validated : Bool } -> Cmd msg



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = validated update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { predicate : String
    , category : String
    , term : Maybe String
    , suggestions : { received : Suggestions, selected : List Suggestions.Item }
    , placeholder : String
    , language : Language
    , selection : List Suggestions.Item
    , textInputId : String
    , isValid : Bool
    , isKeyboardNavigation : Bool
    }


type alias Flags =
    { predicate : String
    , category : String
    , placeholder : String
    , id : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { predicate = flags.predicate
      , category = flags.category
      , term = Nothing
      , suggestions = { received = Suggestions.Empty, selected = [] }
      , placeholder = flags.placeholder
      , language = Translation.NL
      , selection = []
      , textInputId = flags.id
      , isValid = False
      , isKeyboardNavigation = False
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | OnInput String
    | OnInputFromSuggestions String
    | OnChangeFromSuggestions (List String)
    | Escape
    | Backspace
    | SuggestUp
    | SuggestDown
    | ClearSuggestions
    | ConnectCurrent
    | Connect Suggestions.Item
    | Remove Suggestions.Item
    | RequestSuggestions String
    | GotSuggestions String (Result Http.Error (List Suggestions.Item))


validated : (Msg -> Model -> ( Model, Cmd Msg )) -> Msg -> Model -> ( Model, Cmd Msg )
validated updater msg model =
    let
        ( model_, cmd ) =
            updater msg model

        isValid =
            List.length model_.selection >= 1
    in
    ( { model_ | isValid = isValid }
    , Cmd.batch [ portValues { validated = isValid }, cmd ]
    )


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
            , case term of
                Nothing ->
                    Cmd.none

                Just t ->
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

        OnChangeFromSuggestions [] ->
            ( { model
                | suggestions = { received = Suggestions.Empty, selected = [] }
                , term = Nothing
              }
            , focus model.textInputId
            )

        OnChangeFromSuggestions changes ->
            case model.isKeyboardNavigation of
                True ->
                    ( { model | isKeyboardNavigation = False }, Cmd.none )

                False ->
                    let
                        selection =
                            changes
                                |> List.filterMap (Suggestions.itemByIdFrom model.suggestions.received)
                    in
                    ( { model
                        | selection = model.selection ++ selection
                        , suggestions = { received = Suggestions.Empty, selected = [] }
                        , term = Nothing
                      }
                    , focus model.textInputId
                    )

        Escape ->
            ( { model
                | suggestions = { received = Suggestions.Empty, selected = [] }
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
                        , suggestions =
                            { received = Suggestions.Empty
                            , selected = []
                            }
                      }
                    , Cmd.batch
                        [ delayedRequestSuggestions term_
                        , focus model.textInputId
                        ]
                    )

        SuggestUp ->
            ( { model
                | suggestions =
                    { received = Suggestions.previous model.suggestions.received
                    , selected = model.suggestions.selected
                    }
                , isKeyboardNavigation = True
              }
            , Cmd.none
            )

        SuggestDown ->
            ( { model
                | suggestions =
                    { received = Suggestions.next model.suggestions.received
                    , selected = model.suggestions.selected
                    }
                , isKeyboardNavigation = True
              }
            , Cmd.none
            )

        ConnectCurrent ->
            let
                selected =
                    case model.suggestions.received of
                        Suggestions.HasFocus zipper ->
                            Zipper.current zipper :: model.suggestions.selected

                        _ ->
                            model.suggestions.selected
            in
            ( { model
                | selection = model.selection ++ selected
                , suggestions = { received = Suggestions.Empty, selected = [] }
                , term = Nothing
              }
            , focus model.textInputId
            )

        Connect item ->
            ( { model
                | selection = model.selection ++ [ item ]
                , suggestions = { received = Suggestions.Empty, selected = [] }
                , term = Nothing
              }
            , focus model.textInputId
            )

        Remove item ->
            ( { model
                | selection = List.filter ((/=) item) model.selection
              }
            , focus model.textInputId
            )

        ClearSuggestions ->
            ( { model
                | suggestions = { received = Suggestions.Empty, selected = [] }
              }
            , focus model.textInputId
            )

        RequestSuggestions term ->
            if model.term == Just term then
                ( model
                , Edge.requestSuggestions (GotSuggestions term) model.language model.category term
                )

            else
                ( model, Cmd.none )

        GotSuggestions _ (Ok []) ->
            ( { model
                | suggestions = { received = Suggestions.Empty, selected = [] }
              }
            , Cmd.none
            )

        GotSuggestions term (Ok suggestions) ->
            let
                isMostRecent =
                    Just term == model.term
            in
            if isMostRecent then
                case List.filter (\s -> not (List.member s model.selection)) suggestions of
                    hd :: rest ->
                        ( { model
                            | suggestions =
                                { received = Suggestions.NoFocus hd rest
                                , selected = []
                                }
                          }
                        , focus <| viewSuggestionsId model.textInputId
                        )

                    _ ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )

        GotSuggestions _ (Err _) ->
            ( model, Cmd.none )


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


inputKeyDecoder : String -> Decode.Decoder ( Msg, Bool )
inputKeyDecoder key =
    case key of
        "Escape" ->
            Decode.succeed ( ClearSuggestions, True )

        "Enter" ->
            Decode.succeed ( ConnectCurrent, True )

        " " ->
            Decode.succeed ( ConnectCurrent, True )

        "ArrowUp" ->
            Decode.succeed ( SuggestUp, True )

        "ArrowDown" ->
            Decode.succeed ( SuggestDown, True )

        _ ->
            Decode.fail "Unhandled key"


view : Model -> Html Msg
view model =
    let
        alwaysStop x =
            ( x, True )

        classes =
            if model.isValid then
                "form-control"

            else
                "form-control alert-danger"
    in
    div
        [ class "edit-edges" ]
        [ div []
            [ input
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
            , viewSelection model.textInputId model.selection
            ]
        , viewSuggestions model.textInputId model.language model.suggestions.received
        ]


viewSelection : String -> List Suggestions.Item -> Html Msg
viewSelection baseId selection =
    let
        viewItem (( maybeRscId, title ) as item) =
            let
                rscIdStr =
                    Maybe.withDefault title <|
                        Maybe.map Ginger.Id.toString maybeRscId

                id_ =
                    baseId ++ "[" ++ rscIdStr ++ "]"
            in
            li
                [ class "form-control selected"
                ]
                [ input
                    [ type_ "checkbox"
                    , id id_
                    , name baseId
                    , value rscIdStr
                    , checked True
                    , onClick (Remove item)
                    ]
                    []
                , label [ for id_ ] [ text title ]
                , i [ class "remove", onClick (Remove item) ] []
                ]
    in
    ul [ class "selection selection-buttons" ] <|
        List.map viewItem selection


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
            select [ id_, classes, wiring, multiple True, onChange OnChangeFromSuggestions ] <|
                List.map (viewSuggestion language False) (hd :: rest)

        Suggestions.HasFocus suggestions_ ->
            select [ id_, classes, wiring, multiple True, onChange OnChangeFromSuggestions ] <|
                List.concat
                    [ List.map (viewSuggestion language False) (Zipper.before suggestions_)
                    , [ viewSuggestion language True (Zipper.current suggestions_) ]
                    , List.map (viewSuggestion language False) (Zipper.after suggestions_)
                    ]


onChange : (List String -> msg) -> Html.Attribute msg
onChange tagger =
    on "change" (Decode.map tagger selectedOptionsDecoder)


selectedOptionsDecoder : Decode.Decoder (List String)
selectedOptionsDecoder =
    let
        filterSelected options =
            options
                |> List.filter .selected
                |> List.map .value
    in
    Decode.field "target" optionsDecoder
        |> Decode.map filterSelected



{- |
   To allow for large multi selects the length is first retrieved in a IE safe way.
   Then a method that compiles to a JS loop is used to retrieve the options.
-}


optionsDecoder : Decode.Decoder (List Option)
optionsDecoder =
    Decode.field "options"
        (Decode.keyValuePairs (Decode.maybe optionDecoder)
            |> Decode.andThen
                (List.filterMap Tuple.second
                    >> Decode.succeed
                )
        )


type alias Option =
    { value : String, text : String, selected : Bool }


optionDecoder : Decode.Decoder Option
optionDecoder =
    Decode.map3 Option
        (Decode.field "value" Decode.string)
        (Decode.field "text" Decode.string)
        (Decode.field "selected" Decode.bool)


viewSuggestion : Language -> Bool -> Suggestions.Item -> Html Msg
viewSuggestion language isSelected (( maybeResource, title ) as suggestion) =
    let
        c =
            case maybeResource of
                Nothing ->
                    "search-suggestion search-suggestion-new"

                _ ->
                    "search-suggestion"
    in
    option
        [ -- selected isSelected <== this confuses the mechanism currently, need to investigate
          Maybe.map Ginger.Id.toString maybeResource
            |> Maybe.withDefault title
            |> value
        , class c
        ]
        [ text title
        ]
