module AddReference exposing (main)

import Api
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task
import Types exposing (PageId)


main : Program PageId Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { referenceUrl : String
    , pageId : PageId
    }


init : PageId -> ( Model, Cmd Msg )
init pageId =
    ( { referenceUrl = ""
      , pageId = pageId
      }
    , Cmd.none
    )


type Msg
    = UpdateReferenceUrl String
    | AddReferenceUrl
    | AddedReferenceUrl


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateReferenceUrl newReferenceUrl ->
            ( { model | referenceUrl = newReferenceUrl }
            , Cmd.none
            )

        AddReferenceUrl ->
            ( { model | referenceUrl = "" }
            , Api.deleteAbouts model.pageId
                |> Task.andThen (always <| Api.addReferenceUrl model.pageId model.referenceUrl)
                |> Task.attempt (always AddedReferenceUrl)
            )

        AddedReferenceUrl ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "row" ]
        [ div [ class "form-group col-xs-12" ]
            [ div []
                [ label [ class "control-label" ] [ text "Bron toevoegen (optioneel)" ]
                , p [ class "control-label-summary" ]
                    [ text "Voeg hier de URL toe van bijvoorbeeld een video, artikel of website waar je bijdrage over gaat. Deze bron wordt onder je titel in het groene vlak geplaatst." ]
                ]
            , p []
                [ input
                    [ type_ "text"
                    , onInput UpdateReferenceUrl
                    , class "ltr intro form-control"
                    , value model.referenceUrl
                    ]
                    []
                ]
            , a
                [ class "btn btn-small btn-add-thing"
                , classList [ ( "disabled", String.length model.referenceUrl == 0 ) ]
                , onClick AddReferenceUrl
                , href "#"
                ]
                [ text "+ Bovenstaande URL als bron toevoegen" ]
            ]
        ]
