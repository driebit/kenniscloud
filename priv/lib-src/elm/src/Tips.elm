module Tips exposing (main)

import Browser
import Ginger.Id as Id exposing (ResourceId)
import Ginger.Media
import Ginger.Resource as Resource exposing (Resource)
import Ginger.Translation as Translation exposing (Translation)
import Ginger.Util
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Http
import Icon
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Url
import Url.Builder
import Util



-- MAIN


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { tips : List Tip
    , user : User
    , pageId : Int
    , tipEditor : TipEditor
    }


type TipEditor
    = Hidden
    | New String
    | Saving String
    | Error String Http.Error


init : Int -> ( Model, Cmd Msg )
init id =
    ( { tips = []
      , user = Anonymous
      , pageId = id
      , tipEditor = Hidden
      }
    , requestGetTips id
    )



-- UPDATE


type Msg
    = GotTips (Result Http.Error ( User, List Tip ))
    | GotSaveTips (Result ( String, Http.Error ) ( User, List Tip ))
    | GotDeleteTip (Result Http.Error ())
    | SetTipEditor TipEditor
    | SaveTip String
    | DeleteTip ResourceId


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTips (Ok ( user, tips )) ->
            ( { model | tips = tips, user = user }, Cmd.none )

        GotTips (Err _) ->
            ( model, Cmd.none )

        GotSaveTips (Ok ( user, tips )) ->
            ( { model | tips = tips, user = user, tipEditor = Hidden }, Cmd.none )

        GotSaveTips (Err ( val, err )) ->
            ( { model | tipEditor = Error val err }, Cmd.none )

        GotDeleteTip (Ok _) ->
            ( model, requestGetTips model.pageId )

        GotDeleteTip (Err err) ->
            ( model, Cmd.none )

        SetTipEditor editor ->
            ( { model | tipEditor = editor }, Cmd.none )

        SaveTip val ->
            ( { model | tipEditor = Saving val }, requestPostTip model.pageId val )

        DeleteTip id ->
            ( model, requestDeleteTip id )



-- VIEW


view : Model -> Html Msg
view model =
    let
        tips =
            Html.Keyed.ul [ class "community-tips__list" ] <|
                List.map (\x -> ( Id.toString x.id, viewCard model.user x )) model.tips
    in
    Ginger.Util.viewIfNot (List.isEmpty model.tips && isAnonymous model.user) <|
        \_ ->
            section [ class "page-tips" ]
                [ div [ class "main-container--related" ]
                    [ h3 [ class "bordered-title" ] [ text "Tips van de community" ]
                    , case model.tipEditor of
                        Saving val ->
                            div [ class "community-tips" ]
                                [ text ("Bezig met opslaan van: " ++ val)
                                , tips
                                ]

                        Error val _ ->
                            div [ class "community-tips" ]
                                [ p []
                                    [ text ("Er is iets misgegaan bij het opslaan van: " ++ val)
                                    ]
                                , div [ class "community-tips__error" ]
                                    [ button [ class "btn--secondary", onClick (SetTipEditor Hidden) ] [ text "Annuleren" ]
                                    , button [ class "btn--primary", onClick (SetTipEditor (New val)) ] [ text "Probeer opnieuw" ]
                                    ]
                                , tips
                                ]

                        Hidden ->
                            div [ class "community-tips" ]
                                [ Ginger.Util.viewIfNot (isAnonymous model.user) <|
                                    \_ ->
                                        button
                                            [ class "btn--primary community-tips__new"
                                            , onClick (SetTipEditor (New ""))
                                            ]
                                            [ text "Voeg jouw bron toe" ]
                                , tips
                                ]

                        New val ->
                            div [ class "community-tips" ]
                                [ Html.form [ class "community-tips__form", onSubmit (SaveTip val) ]
                                    [ label [ for "tip-input" ] [ text "Plaats hier de link naar je bron" ]
                                    , input [ id "tip-input", type_ "url", value val, onInput (SetTipEditor << New) ] []
                                    , div []
                                        [ input [ class "btn--secondary", type_ "button", onClick (SetTipEditor Hidden), value "Annuleren" ] []
                                        , button [ class "btn--primary" ] [ text "Plaatsen" ]
                                        ]
                                    ]
                                , tips
                                ]
                    ]
                ]


isAnonymous : User -> Bool
isAnonymous user =
    case user of
        Anonymous ->
            True

        _ ->
            False


viewCard : User -> Tip -> Html Msg
viewCard user tip =
    let
        formatUrl url =
            case Url.fromString url of
                Nothing ->
                    url

                Just x ->
                    x.host
    in
    li [ class "community-tips__tip list-item-small" ]
        [ Ginger.Util.viewMaybe tip.author <|
            \{ name } ->
                span [ class "community-tips__author" ]
                    [ Translation.textNL name ]
        , div [ class "list-item-small__img" ]
            [ Ginger.Util.viewMaybe
                (if tip.og_image == Nothing then
                    tip.depictionUrl

                 else
                    tip.og_image
                )
                (\url -> img [ src url ] [])
            ]
        , div [ class "list-item-small__content" ]
            [ div []
                [ small [] [ text (formatUrl tip.url) ]
                , tip.og_title
                    |> Maybe.map (\title -> h5 [] [ text title ])
                    |> Maybe.withDefault (Translation.textNL tip.title)
                ]
            , a [ target "_blank", rel "noopener noreferrer", href tip.url ] [ span [] [ text "Lees meer" ] ]
            , Ginger.Util.viewMaybe tip.author <|
                viewDelete tip.id user
            ]
        ]


viewDelete : ResourceId -> User -> TipAuthor -> Html Msg
viewDelete tipId user author =
    let
        button_ =
            button
                [ onClick (DeleteTip tipId)
                , class "community-tips__delete"
                ]
                [ Icon.view Icon.Close ]
    in
    case user of
        CollabManager { userId } ->
            button_

        PageAuthor { userId } ->
            Ginger.Util.viewIf (userId == author.id) <|
                \_ -> button_

        Member { userId } ->
            Ginger.Util.viewIf (userId == author.id) <|
                \_ -> button_

        _ ->
            text ""


decodeNonEmptyString : Decode.Decoder (Maybe String)
decodeNonEmptyString =
    Decode.string
        |> Decode.andThen
            (\s ->
                if String.isEmpty s then
                    Decode.succeed Nothing

                else
                    Decode.succeed (Just s)
            )



-- HTTP


type User
    = Anonymous
    | PageAuthor { userId : ResourceId }
    | CollabManager { userId : ResourceId }
    | Member { userId : ResourceId }


type alias TipAuthor =
    { id : ResourceId
    , name : Translation
    }


type alias Tip =
    { id : ResourceId
    , title : Translation
    , author : Maybe TipAuthor
    , og_title : Maybe String
    , og_image : Maybe String
    , depictionUrl : Maybe String
    , url : String
    }


requestGetTips : Int -> Cmd Msg
requestGetTips id =
    Http.get
        { url =
            Url.Builder.absolute
                [ "api"
                , "model"
                , "tips"
                , "get"
                , "for"
                , String.fromInt id
                ]
                []
        , expect = Http.expectJson GotTips decodeTipsData
        }


requestPostTip : Int -> String -> Cmd Msg
requestPostTip id url =
    Http.post
        { url =
            Url.Builder.absolute
                [ "api"
                , "model"
                , "tips"
                , "post"
                , String.fromInt id
                ]
                []
        , body =
            Http.jsonBody <|
                Encode.object [ ( "uri", Encode.string url ) ]
        , expect =
            Http.expectJson
                (GotSaveTips << Result.mapError (Tuple.pair url))
                decodeTipsData
        }


requestDeleteTip : ResourceId -> Cmd Msg
requestDeleteTip tipId =
    Http.request
        { method = "DELETE"
        , headers = []
        , url =
            Url.Builder.absolute
                [ "api"
                , "model"
                , "tips"
                , "delete"
                , String.fromInt (Id.toInt tipId)
                ]
                []
        , body = Http.emptyBody
        , expect = Http.expectWhatever GotDeleteTip
        , timeout = Nothing
        , tracker = Nothing
        }


decodeTipsData : Decode.Decoder ( User, List Tip )
decodeTipsData =
    Decode.field "result"
        (Decode.succeed Tuple.pair
            |> Pipeline.required "loggedInUser" decodeUser
            |> Pipeline.required "tips" (Decode.list decodeTip)
        )


decodeTip : Decode.Decoder Tip
decodeTip =
    Decode.succeed Tip
        |> Pipeline.required "id" Id.fromJson
        |> Pipeline.required "title" Util.decodeTranslation
        |> Pipeline.required "author" (Decode.nullable decodeTipAuthor)
        |> Pipeline.optional "og_title" decodeNonEmptyString Nothing
        |> Pipeline.optional "og_image" decodeNonEmptyString Nothing
        |> Pipeline.optional "depiction_url" decodeNonEmptyString Nothing
        |> Pipeline.required "uri" Decode.string


decodeUser : Decode.Decoder User
decodeUser =
    let
        toUser id role =
            case role of
                "page_author" ->
                    PageAuthor { userId = id }

                "collab_manager" ->
                    CollabManager { userId = id }

                "member" ->
                    Member { userId = id }

                _ ->
                    Anonymous
    in
    Decode.oneOf
        [ Decode.succeed toUser
            |> Pipeline.required "id" Id.fromJson
            |> Pipeline.required "role" Decode.string
        , Decode.succeed Anonymous
        ]


decodeTipAuthor : Decode.Decoder TipAuthor
decodeTipAuthor =
    Decode.succeed TipAuthor
        |> Pipeline.required "authorId" Id.fromJson
        |> Pipeline.required "title" Util.decodeTranslation
