port module Crowd exposing (main)

import Browser
import Browser.Dom as Browser
import Browser.Events as Browser
import Browser.Navigation
import Crowd.Assets as Assets
import Crowd.Graph.Camera as Camera exposing (Camera)
import Crowd.Graph.Mouse as Mouse exposing (MouseMoveData)
import Crowd.Graph.Render as Render
import Crowd.Graph.Simulation as Simulation
import Crowd.Graph.TextureStore as TextureStore exposing (TextureStore, TextureUrl)
import Crowd.Graph.View.IconButton as IconButton
import Crowd.Graph.View.PersonCard as PersonCard
import Crowd.Mode exposing (Mode(..))
import Crowd.Request exposing (Tag)
import Crowd.Route as Route exposing (Route)
import Edit.Edge
import Force
import Ginger.Id as Id exposing (ResourceId)
import Ginger.Predicate as Predicate
import Ginger.Resource exposing (Edges, ResourceWith)
import Ginger.Translation as Translation exposing (Translation)
import Ginger.Util exposing (viewIf, viewMaybe)
import Graph exposing (Node, NodeId)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Icon
import IntDict
import Json.Decode as Decode
import Json.Encode as Encode
import Point2d
import QRCode
import Svg.Attributes as SvgA
import Task
import Time exposing (Posix)
import Units.Quantity as Quantity
import Url exposing (Url)
import Url.Builder as Url
import Util


port register : RegistrationData -> Cmd msg


port onTopic : (Decode.Value -> msg) -> Sub msg


type alias RegistrationData =
    { name : String
    , email : String
    , showContact : Bool
    , keywords : List Int
    }



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if Force.isCompleted model.simulation.simulation then
            Sub.none

          else
            Time.every 50 Tick
        , Browser.onResize GotResize
        , onTopic (always GotUpdate)
        ]


type alias Model =
    { url : String
    , host : String
    , route : Maybe Route
    , mouse : Mouse.State
    , crowd : Maybe Crowd.Request.Crowd
    , simulation : Simulation.Model Node
    , textureStore : TextureStore
    , camera : Camera
    , panning : ( Float, Float )
    , zoom : Float
    , selectedNode : Maybe ( NodeId, Node )
    , hoveredNode : Maybe ( NodeId, Node )
    , canvasDimensions : Render.CanvasDimensions
    , editable : Bool
    , registration : Registration
    , email : String
    , showContact : Bool
    , crowdMode : Mode
    , showConfirmationMessage : Bool
    , pendingCrowdRequest : Bool
    }


type Registration
    = None
    | Open ParticipantData (Maybe KeywordsData)
    | Instruct
    | Done
    | Confirmed


type alias ParticipantData =
    Edit.Edge.Model


type alias KeywordsData =
    Edit.Edge.Model


type alias Flags =
    { url : String
    , host : String
    , editableInMeetupMode : Bool
    , mode : String
    , justRegistered : Bool
    }


init : Flags -> ( Model, Cmd Msg )
init { host, url, editableInMeetupMode, mode, justRegistered } =
    let
        route =
            Url.fromString url
                |> Maybe.andThen Route.fromUrl

        crowdMode =
            Crowd.Mode.fromString mode
    in
    case route of
        Just (Route.Crowd id _ crowdLink) ->
            ( { crowd = Nothing
              , url = url
              , host = host
              , route = route
              , simulation = Simulation.init
              , textureStore = TextureStore.empty
              , mouse = Mouse.init
              , zoom = 3000
              , panning = ( 0, 0 )
              , camera = Camera.fixedWidth 2000 ( 0, 0 )
              , selectedNode = Nothing
              , hoveredNode = Nothing
              , canvasDimensions = Render.CanvasDimensions 640 480
              , editable = Crowd.Mode.editable crowdMode editableInMeetupMode
              , registration =
                    if justRegistered then
                        Confirmed

                    else
                        None
              , email = ""
              , showContact = False
              , crowdMode = crowdMode
              , showConfirmationMessage = False
              , pendingCrowdRequest = True
              }
            , Cmd.batch
                [ Task.attempt GotElement (Browser.getElement "crowdviewer")
                , Crowd.Request.run (Crowd.Request.crowd id crowdLink) GotCrowd
                ]
            )

        Nothing ->
            ( { crowd = Nothing
              , url = url
              , host = host
              , route = route
              , simulation = Simulation.init
              , textureStore = TextureStore.empty
              , mouse = Mouse.init
              , zoom = 3000
              , panning = ( 0, 0 )
              , camera = Camera.fixedWidth 2000 ( 0, 0 )
              , selectedNode = Nothing
              , hoveredNode = Nothing
              , canvasDimensions = Render.CanvasDimensions 640 480
              , editable = Crowd.Mode.editable crowdMode editableInMeetupMode
              , registration = None
              , email = ""
              , showContact = False
              , crowdMode = crowdMode
              , showConfirmationMessage = False
              , pendingCrowdRequest = False
              }
            , Cmd.none
            )


type Msg
    = NoOp
    | GotElement (Result Browser.Error Browser.Element)
    | Tick Posix
    | GotCrowd (Result Http.Error Crowd.Request.Crowd)
    | UpdateStore (TextureStore -> TextureStore)
    | MouseMove MouseMoveData
    | MouseClick
    | MouseDown
    | MouseUp
    | Zoom ZoomDirection
    | GotResize Int Int
    | TouchStart Mouse.TouchData
    | TouchEnd Mouse.TouchData
    | TouchMove Mouse.TouchData
    | Unselect
    | AddParticipant
    | SaveParticipant
    | SavedParticipant (Result Http.Error RegistrationResult)
    | ParticipantMsg Edit.Edge.Msg
    | KeywordsMsg Edit.Edge.Msg
    | EmailInput String
    | ShowContactToggle
    | GotUserTags (Result Http.Error (List Tag))
    | GotUpdate


type RegistrationResult
    = RegisteredId ResourceId
    | RegistrationPending String


type ZoomDirection
    = ZoomIn
    | ZoomOut


type Node
    = ParticipantNode Crowd.Request.Participant
    | TagNode Crowd.Request.Tag
    | CrowdNode


getId : Crowd.Request.Crowd -> Node -> Id.ResourceId
getId crowd node =
    case node of
        ParticipantNode participant ->
            Crowd.Request.participantId participant

        TagNode tag ->
            tag.id

        CrowdNode ->
            crowd.id


avatars : List Node -> List TextureUrl
avatars nodes =
    List.filterMap
        (\node ->
            case node of
                ParticipantNode (Crowd.Request.Participant user) ->
                    Maybe.map TextureStore.url (Crowd.Request.getAvatar user)

                ParticipantNode (Crowd.Request.Anonymous _) ->
                    Just (TextureStore.url Assets.tag)

                TagNode _ ->
                    Just (TextureStore.url Assets.tag)

                CrowdNode ->
                    Nothing
        )
        nodes


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotElement (Ok element) ->
            ( { model
                | canvasDimensions =
                    { width = round element.element.width
                    , height = round element.element.height
                    }
              }
            , Cmd.none
            )

        GotElement (Err _) ->
            ( model, Cmd.none )

        GotResize _ _ ->
            ( model, Task.attempt GotElement (Browser.getElement "crowdviewer") )

        Tick t ->
            ( { model
                | simulation = Simulation.tick t model.simulation
                , panning = model.panning
                , camera = Camera.fixedWidth model.zoom model.panning
              }
            , Cmd.none
            )

        UpdateStore f ->
            ( { model | textureStore = f model.textureStore }, Cmd.none )

        GotCrowd result ->
            let
                model_ =
                    { model | pendingCrowdRequest = False }
            in
            case result of
                Ok res ->
                    let
                        users =
                            List.map ParticipantNode res.participants

                        tags =
                            List.map TagNode res.tags

                        nodes =
                            List.map (\node -> ( Id.toInt (getId res node), node )) (CrowdNode :: (users ++ tags))

                        tagConnections =
                            List.concatMap
                                (\user ->
                                    List.map
                                        (\tag ->
                                            { source = Id.toInt (Crowd.Request.participantId user)
                                            , target = Id.toInt tag.id
                                            , targetDistance = 300
                                            , render = True
                                            }
                                        )
                                        (Crowd.Request.participantTags user)
                                )
                                res.participants
                                ++ List.map (\tag -> { source = Id.toInt tag.id, target = Id.toInt res.id, targetDistance = 300, render = False }) res.tags
                                ++ List.map (\participant -> { source = Id.toInt (Crowd.Request.participantId participant), target = Id.toInt res.id, targetDistance = 600, render = False })
                                    (List.filter (List.isEmpty << Crowd.Request.participantTags) res.participants)
                                ++ List.map (\participant -> { source = Id.toInt (Crowd.Request.participantId participant), target = Id.toInt res.id, targetDistance = 400, render = False })
                                    (List.filter (not << List.isEmpty << Crowd.Request.participantTags) res.participants)

                        simulation =
                            Simulation.withGraph
                                (List.map
                                    (\( id, node ) ->
                                        { id = id
                                        , label =
                                            Force.entity id node
                                        }
                                    )
                                    nodes
                                )
                                tagConnections
                                model.simulation
                    in
                    case ( model.crowdMode, res.isElevated, ( model.registration, model.route ) ) of
                        ( DayCrowd, False, _ ) ->
                            -- Link code does not match
                            ( model_, Browser.Navigation.load (Route.toEventUrl model.route) )

                        ( DayCrowd, True, ( _, Just (Route.Crowd _ Route.Present _) ) ) ->
                            -- Link code does match, in presentation mode
                            setSimulation model_ simulation res nodes

                        ( DayCrowd, True, ( Confirmed, _ ) ) ->
                            -- Link code does match, just registered and confirmed
                            setSimulation model_ simulation res nodes

                        ( DayCrowd, True, _ ) ->
                            -- Link code does match, not just registered
                            let
                                model__ =
                                    { model_ | crowd = Just res }

                                ( model___, cmd ) =
                                    update AddParticipant model__
                            in
                            ( model___, cmd )

                        _ ->
                            setSimulation model_ simulation res nodes

                Err _ ->
                    ( model_, Cmd.none )

        TouchStart event ->
            let
                newMouse =
                    Mouse.touchStart event model.mouse
            in
            ( updatePanning newMouse model, Cmd.none )

        TouchEnd event ->
            let
                newMouse =
                    Mouse.touchEnd event model.mouse

                targetNode =
                    findHover model model.mouse

                ( model_, cmd ) =
                    handleMouse model targetNode
            in
            ( { model_ | mouse = newMouse }, cmd )

        TouchMove event ->
            let
                newMouse =
                    Mouse.touchMove event model.mouse
            in
            ( updatePanning newMouse model, Cmd.none )

        MouseMove mouseData ->
            let
                newMouse =
                    Mouse.move mouseData model.mouse
            in
            ( updatePanning newMouse model, Cmd.none )

        Unselect ->
            let
                registration =
                    if model.registration == Instruct then
                        Done

                    else
                        None
            in
            ( { model | selectedNode = Nothing, registration = registration }, Cmd.none )

        Zoom direction ->
            case direction of
                ZoomIn ->
                    let
                        newZoom =
                            model.zoom * 0.8
                    in
                    ( { model | zoom = newZoom, camera = Camera.fixedWidth newZoom model.panning }, Cmd.none )

                ZoomOut ->
                    let
                        newZoom =
                            model.zoom * 1.25
                    in
                    ( { model | zoom = newZoom, camera = Camera.fixedWidth newZoom model.panning }, Cmd.none )

        MouseDown ->
            ( { model | mouse = Mouse.down model.mouse }, Cmd.none )

        MouseUp ->
            ( { model | mouse = Mouse.up model.mouse }, Cmd.none )

        MouseClick ->
            let
                targetNode =
                    findHover model model.mouse
            in
            handleMouse model targetNode

        AddParticipant ->
            case ( model.selectedNode, model.registration ) of
                ( Nothing, None ) ->
                    openAddParticipantDialog model

                _ ->
                    ( model, Cmd.none )

        SaveParticipant ->
            case model.registration of
                Open participant maybeKeywords ->
                    case ( Edit.Edge.entry participant, Edit.Edge.selection participant, model.crowd ) of
                        ( _, ( Just user, _ ) :: _, Just { id } ) ->
                            ( { model | registration = None }
                            , postNewParticipant ( Just user, Nothing ) id maybeKeywords
                            )

                        ( _, ( Nothing, newEntry ) :: _, Just { id } ) ->
                            ( { model | registration = None }
                            , if model.crowdMode == DayCrowd then
                                registerDayCrowdParticipant newEntry model.email model.showContact id maybeKeywords

                              else
                                postNewParticipant ( Nothing, Just ( newEntry, model.email, model.showContact ) ) id maybeKeywords
                            )

                        ( Just newEntry, _, Just { id } ) ->
                            ( { model | registration = None }
                            , if model.crowdMode == DayCrowd then
                                registerDayCrowdParticipant newEntry model.email model.showContact id maybeKeywords

                              else
                                postNewParticipant ( Nothing, Just ( newEntry, model.email, model.showContact ) ) id maybeKeywords
                            )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SavedParticipant (Ok _) ->
            let
                registration =
                    if model.crowdMode == DayCrowd then
                        Instruct

                    else
                        None
            in
            case model.route of
                Just (Route.Crowd id _ crowdLink) ->
                    ( { model | registration = registration }
                    , if model.crowdMode == DayCrowd then
                        Cmd.none

                      else
                        Crowd.Request.run (Crowd.Request.crowd id crowdLink) GotCrowd
                    )

                Nothing ->
                    ( { model | registration = registration }, Cmd.none )

        SavedParticipant (Err _) ->
            -- TODO: Show error message
            ( model, Cmd.none )

        ParticipantMsg m ->
            case model.registration of
                Open participant keywords ->
                    case Edit.Edge.itemConnected participant m of
                        Nothing ->
                            let
                                ( participant_, cmdp ) =
                                    Edit.Edge.update m participant

                                ( keywords_, cmdk ) =
                                    case ( keywords, Edit.Edge.validated participant, model.crowd ) of
                                        ( _, False, _ ) ->
                                            ( Nothing, Cmd.none )

                                        ( Nothing, True, Just crowd ) ->
                                            initKeywords model.crowdMode crowd.tags []

                                        _ ->
                                            ( keywords, Cmd.none )
                            in
                            ( { model | registration = Open participant_ keywords_ }
                            , Cmd.batch [ Cmd.map ParticipantMsg cmdp, cmdk ]
                            )

                        Just ( Nothing, _ ) ->
                            let
                                ( participant_, cmdp ) =
                                    Edit.Edge.update m participant
                            in
                            ( { model | registration = Open participant_ keywords }
                            , Cmd.map ParticipantMsg cmdp
                            )

                        Just ( Just user, _ ) ->
                            let
                                ( participant_, cmdp ) =
                                    Edit.Edge.update m participant
                            in
                            ( { model | registration = Open participant_ keywords }
                            , Cmd.batch [ Cmd.map ParticipantMsg cmdp, Crowd.Request.tagsOf user GotUserTags ]
                            )

                _ ->
                    ( model, Cmd.none )

        KeywordsMsg m ->
            case model.registration of
                Open _ Nothing ->
                    ( model, Cmd.none )

                Open participant (Just keywords) ->
                    let
                        ( keywords_, cmd ) =
                            Edit.Edge.update m keywords
                    in
                    ( { model | registration = Open participant (Just keywords_) }
                    , Cmd.map KeywordsMsg cmd
                    )

                _ ->
                    ( model, Cmd.none )

        EmailInput email ->
            ( { model | email = email }, Cmd.none )

        ShowContactToggle ->
            ( { model | showContact = not model.showContact }, Cmd.none )

        GotUserTags (Ok tags) ->
            case model.registration of
                Open participant _ ->
                    let
                        ( keywords, cmd ) =
                            case model.crowd of
                                Just crowd ->
                                    initKeywords model.crowdMode crowd.tags <|
                                        List.foldl
                                            (\i a ->
                                                if List.member i crowd.tags then
                                                    i :: a

                                                else
                                                    a
                                            )
                                            []
                                            tags

                                Nothing ->
                                    initKeywords model.crowdMode [] []
                    in
                    ( { model | registration = Open participant keywords }, cmd )

                _ ->
                    ( model, Cmd.none )

        GotUserTags (Err _) ->
            ( model, Cmd.none )

        GotUpdate ->
            ( model
            , case ( model.pendingCrowdRequest, model.route ) of
                ( False, Just (Route.Crowd id _ crowdLink) ) ->
                    Crowd.Request.run (Crowd.Request.crowd id crowdLink) GotCrowd

                _ ->
                    Cmd.none
            )


setSimulation model simulation res nodes =
    ( { model
        | simulation = simulation
        , crowd = Just res
      }
    , TextureStore.preloadAllNonPowerOfTwo
        TextureStore.empty
        ([ TextureStore.url Assets.tag
         , TextureStore.url Assets.person
         , TextureStore.url Assets.meetup
         ]
            ++ (avatars <| List.map Tuple.second nodes)
        )
        UpdateStore
    )


registerDayCrowdParticipant : String -> String -> Bool -> ResourceId -> Maybe KeywordsData -> Cmd Msg
registerDayCrowdParticipant name email showContact crowd maybeKeywords =
    let
        keywords =
            case maybeKeywords of
                Nothing ->
                    []

                Just ks ->
                    Edit.Edge.selection ks
                        |> List.filterMap Tuple.first
                        |> List.map Id.toInt

        data =
            RegistrationData name email showContact keywords
    in
    register data


postNewParticipant : ( Maybe ResourceId, Maybe ( String, String, Bool ) ) -> ResourceId -> Maybe KeywordsData -> Cmd Msg
postNewParticipant ( maybeRsc, maybeNewEntry ) crowd maybeKeywords =
    let
        keywords =
            case maybeKeywords of
                Nothing ->
                    []

                Just ks ->
                    List.filterMap Tuple.first (Edit.Edge.selection ks)

        fields =
            case ( maybeRsc, maybeNewEntry ) of
                ( Just user, _ ) ->
                    [ ( "user", Id.toJson user )
                    , ( "crowd", Encode.int (Id.toInt crowd) )
                    , ( "subject", Encode.list Id.toJson keywords )
                    ]

                ( Nothing, Just ( name, email, showContact ) ) ->
                    [ ( "name", Encode.string name )
                    , ( "email", Encode.string email )
                    , ( "show_contact", Encode.bool showContact )
                    , ( "crowd", Encode.int (Id.toInt crowd) )
                    , ( "subject", Encode.list Id.toJson keywords )
                    ]

                ( Nothing, Nothing ) ->
                    -- TODO: Impossible state like this should be made impossible.
                    []
    in
    Http.post
        { url = "/api/model/crowdparticipant/post/data/"
        , body = Http.jsonBody (Encode.object fields)
        , expect = Http.expectJson SavedParticipant registrationDecoder
        }


registrationDecoder =
    Decode.oneOf
        [ Decode.map RegisteredId (Decode.field "id" Id.fromJson)
        , Decode.map RegistrationPending (Decode.field "id" Decode.string)
        ]


handleMouse : Model -> Maybe ( NodeId, Node ) -> ( Model, Cmd Msg )
handleMouse model targetNode =
    case ( model.mouse.hasDragged, targetNode ) of
        ( False, Just ( id, _ ) ) ->
            if model.registration == None || model.registration == Confirmed then
                ( { model
                    | selectedNode = targetNode
                    , hoveredNode = targetNode
                    , simulation = Simulation.attract id 0.2 model.simulation
                  }
                , Cmd.none
                )

            else
                ( { model | selectedNode = Nothing }, Cmd.none )

        ( False, _ ) ->
            ( { model | selectedNode = Nothing }, Cmd.none )

        ( True, _ ) ->
            ( model, Cmd.none )


openAddParticipantDialog model =
    let
        ( newParticipant, cmd ) =
            initParticipant model.crowdMode
    in
    ( { model
        | selectedNode = Nothing
        , registration = Open newParticipant Nothing
        , email = ""
      }
    , Cmd.map ParticipantMsg cmd
    )


initParticipant : Mode -> ( ParticipantData, Cmd Edit.Edge.Msg )
initParticipant crowdMode =
    Edit.Edge.init
        { category = "Person"
        , textInputId = "add_participant"
        , label = Just (Crowd.Mode.addParticipantLabel crowdMode)
        , placeholder = "Zoek op naam of voeg toe"
        , subject = Nothing
        , predicate = "rsvp"
        , minimum = Just 1
        , maximum = Just 1
        , allowNew = Just "Voeg tijdelijke deelnemer toe"
        , checkboxes = False
        , suggestions = []
        , selection = []
        , focus = True
        , error = Nothing
        , allowExisting = Crowd.Mode.allowAddExistingMembers crowdMode
        }


initKeywords : Mode -> List { id : ResourceId, title : Translation } -> List { id : ResourceId, title : Translation } -> ( Maybe KeywordsData, Cmd Msg )
initKeywords crowdMode crowdTags userTags =
    let
        ( k, c ) =
            Edit.Edge.init
                { category = "Keyword"
                , textInputId = "add_keywords"
                , label = Just (Crowd.Mode.keywordLabel crowdMode)
                , placeholder = "Typ een letter om te zoeken"
                , subject = Nothing
                , predicate = "subject"
                , minimum = Just 1
                , maximum = Nothing
                , allowNew = Nothing
                , checkboxes = True
                , suggestions = Edit.Edge.selectionFromResources Translation.NL crowdTags
                , selection = Edit.Edge.selectionFromResources Translation.NL userTags
                , focus = False
                , error = Nothing
                , allowExisting = Crowd.Mode.allowAddExistingMembers crowdMode
                }
    in
    ( Just k, Cmd.map KeywordsMsg c )


updatePanning : Mouse.State -> Model -> Model
updatePanning newMouse model =
    let
        targetNode =
            findHover model newMouse

        newPanning =
            let
                ( panX, panY ) =
                    model.panning

                ( dx, dy ) =
                    Mouse.delta newMouse
            in
            if model.mouse.isDown then
                ( panX - toFloat dx * model.zoom / toFloat model.canvasDimensions.width
                , panY + toFloat dy * model.zoom / toFloat model.canvasDimensions.width
                )

            else
                ( panX, panY )
    in
    { model
        | mouse = newMouse
        , panning = newPanning
        , hoveredNode = targetNode
        , camera = Camera.fixedWidth model.zoom newPanning
    }


findHover : Model -> Mouse.State -> Maybe ( NodeId, Node )
findHover model mouse =
    let
        ( mx, my ) =
            Mouse.position mouse

        ( x, y ) =
            Camera.viewportToGameCoordinates model.camera
                ( model.canvasDimensions.width
                , model.canvasDimensions.height
                )
                ( mx, my )
    in
    Graph.nodes model.simulation.graph
        |> List.filter
            (\node ->
                abs (node.label.x - x) + abs (node.label.y - y) < nodeRadius node.label.value model + 10
            )
        |> List.head
        |> Maybe.map
            (\node -> ( node.label.id, node.label.value ))



-- VIEW


view : Model -> Html Msg
view model =
    case model.route of
        Just route ->
            case route of
                Route.Crowd id Route.Normal crowdLink ->
                    div
                        [ Html.Attributes.id "crowdviewer"
                        , class "crowd-wrapper -normal with-elm"
                        , class (Crowd.Mode.className model.crowdMode)
                        ]
                        [ viewControls model (Route.Crowd id Route.Present crowdLink)
                        , viewGraph model
                        ]

                Route.Crowd id Route.Present crowdLink ->
                    div
                        [ Html.Attributes.id "crowdviewer"
                        , class "crowd-wrapper -present with-elm"
                        ]
                        [ Ginger.Util.viewMaybe model.crowd <|
                            \crowd -> viewPresentHeader model crowd id crowdLink
                        , viewGraph model
                        , case model.crowdMode of
                            DayCrowd ->
                                img
                                    [ src (Assets.imageUrl "bibliotheek-crowd-logo.svg")
                                    , class "crowd-bieb-logo"
                                    ]
                                    []

                            _ ->
                                viewQr model.url
                        ]

        Nothing ->
            div [] [ text "Invalid mode selected" ]


viewPresentHeader : Model -> Crowd.Request.Crowd -> ResourceId -> Maybe Route.CrowdLink -> Html Msg
viewPresentHeader model crowd id crowdLink =
    case model.crowdMode of
        DayCrowd ->
            div [ class "crowd-present-header-wrapper" ]
                [ div [ class "crowd-present-header" ]
                    [ h2
                        []
                        [ Translation.text Translation.NL crowd.title
                        , span [ class "crowd-date" ] [ text "" ] -- TODO: feed actual date here
                        ]
                    , viewControls model (Route.Crowd id Route.Normal crowdLink)
                    , div [ class "crowd-present-qr-wrapper" ]
                        [ viewQr (Route.toUrlWithHost model.host (Route.Crowd id Route.Normal crowdLink))
                        , div []
                            [ h4 [] [ text "Doe mee met de crowd en kom in contact met de mensen om je heen!" ]
                            , p [] [ text "Scan de QR code en geef je op. Je gegevens worden na vandaag volledig gewist." ]
                            ]
                        ]
                    ]
                ]

        _ ->
            div [ class "crowd-present-header-wrapper" ]
                [ div [ class "crowd-present-header" ]
                    [ h4 []
                        [ a [ href "/", class "global-nav__logo" ]
                            [ img [ src "/lib/images/cloud.svg", alt "" ] []
                            , text "KennisCloud"
                            , span [ class "crowd-present-header-wrapper" ] [ text " Meetup" ]
                            ]
                        ]
                    , h2 [] [ Translation.text Translation.NL crowd.title ]
                    , viewControls model (Route.Crowd id Route.Normal crowdLink)
                    ]
                ]


viewControls : Model -> Route -> Html Msg
viewControls model route =
    let
        zoombuttons =
            [ IconButton.view IconButton.Plus (Zoom ZoomIn)
            , IconButton.view IconButton.Minus (Zoom ZoomOut)
            ]

        buttons =
            case model.route of
                Just (Route.Crowd _ Route.Present _) ->
                    [ IconButton.view IconButton.PlusPresent (Zoom ZoomIn)
                    , IconButton.view IconButton.MinusPresent (Zoom ZoomOut)
                    , IconButton.viewLink IconButton.FullscreenPresent route
                    ]

                _ ->
                    [ IconButton.view IconButton.Plus (Zoom ZoomIn)
                    , IconButton.view IconButton.Minus (Zoom ZoomOut)
                    , IconButton.viewLink IconButton.Fullscreen route
                    ]

        offlineButtons =
            case model.route of
                Just (Route.Crowd _ _ Nothing) ->
                    []

                _ ->
                    [ br [] []
                    , case ( model.registration, model.editable ) of
                        ( None, True ) ->
                            button [ class "btn btn--dark", onClick AddParticipant ] [ text (Crowd.Mode.buttonText model.crowdMode) ]

                        _ ->
                            text ""
                    ]
    in
    div [ class "crowd-controls" ]
        [ case model.crowdMode of
            DayCrowd ->
                case model.route of
                    Just (Route.Crowd _ Route.Present _) ->
                        text ""

                    _ ->
                        div [ class "crowd-control-buttons" ] zoombuttons

            _ ->
                div [ class "crowd-control-buttons" ] (buttons ++ offlineButtons)
        , case model.registration of
            Open participant keywords ->
                case model.crowdMode of
                    DayCrowd ->
                        viewNewParticipantDayCrowd model.email model.showContact participant keywords

                    _ ->
                        viewNewParticipant model.email participant keywords

            Instruct ->
                viewConfirmationInstruction

            _ ->
                text ""
        , case model.selectedNode of
            Just ( _, ParticipantNode (Crowd.Request.Participant user) ) ->
                aside []
                    [ if model.crowdMode == DayCrowd then
                        PersonCard.dayCrowdView Unselect user

                      else
                        PersonCard.view Unselect user
                    ]

            _ ->
                text ""
        ]


viewConfirmationInstruction =
    aside []
        [ div [ class "list__item person-card" ]
            [ button [ class "person-card-close", onClick Unselect ] [ Icon.view Icon.Close ]
            , article []
                [ p [ style "font-weight" "bold" ]
                    [ text "Bijna klaar!" ]
                , p []
                    [ text "Er is een bevestigingsverzoek verstuurd naar het opgegeven emailadres."
                    ]
                ]
            ]
        ]


viewNewParticipant : String -> Edit.Edge.Model -> Maybe Edit.Edge.Model -> Html Msg
viewNewParticipant email participant keywords =
    let
        newParticipantFormatter value =
            "Niet-lid \"" ++ value ++ "\""

        showEmail =
            case participant.selection of
                [] ->
                    True

                ( _, ( Nothing, _ ) ) :: _ ->
                    True

                _ ->
                    False
    in
    aside []
        [ div [ class "list__item person-card" ]
            [ button [ class "person-card-close", onClick Unselect ] [ Icon.view Icon.Close ]
            , a []
                [ article []
                    [ div [ class "list__item__image" ]
                        [ img [ src Assets.person ] []
                        ]
                    , div [ class "list__item__content" ]
                        [ div [ class "list__item__title" ]
                            [ div [ class "category-of" ]
                                [ div [ class "category-of__cat" ]
                                    [ text "Deelnemer" ]
                                ]
                            , div [] [ Html.map ParticipantMsg <| Edit.Edge.view (Just newParticipantFormatter) participant ]
                            , viewIf showEmail
                                (\_ ->
                                    div []
                                        [ label [ for "participant-email" ] [ text "E-mailadres (optioneel):" ]
                                        , input [ id "participant-email", placeholder "Voer e-mailadres in", class "form-control", value email, onInput EmailInput ] []
                                        ]
                                )
                            ]
                        , viewMaybe keywords
                            (\k -> p [] [ Html.map KeywordsMsg <| Edit.Edge.view Nothing k ])
                        , viewMaybe keywords
                            (\k ->
                                viewIf (Edit.Edge.validated k)
                                    (\_ ->
                                        p [] [ button [ onClick SaveParticipant ] [ text "Voeg toe aan deze crowd" ] ]
                                    )
                            )
                        ]
                    ]
                ]
            ]
        ]


viewNewParticipantDayCrowd : String -> Bool -> Edit.Edge.Model -> Maybe Edit.Edge.Model -> Html Msg
viewNewParticipantDayCrowd email showContact participant keywords =
    let
        newParticipantFormatter value =
            "Niet-lid \"" ++ value ++ "\""

        showEmail =
            case participant.selection of
                [] ->
                    True

                ( _, ( Nothing, _ ) ) :: _ ->
                    True

                _ ->
                    False
    in
    aside []
        [ div [ class "list__item person-card wide" ]
            [ button [ class "person-card-close", onClick Unselect ] [ Icon.view Icon.Close ]
            , article []
                [ div [ class "list__item__content" ]
                    [ div [ class "list__item__intro" ]
                        [ h2 [] [ text "Sluit je aan bij de dagcrowd!" ]
                        , p [ class "emphasis" ]
                            [ text "Maak het mogelijk contact met je mede-bezoekers te kunnen opnemen:"
                            , br [] []
                            , text "Meld je aan!"
                            ]
                        , p []
                            [ text "Je bent met je gegevens op het grote scherm in deze ruimte zichtbaar voor je mededeelnemers van vandaag." ]
                        , p []
                            [ text "De crowd is alleen vandaag te bekijken."
                            , br [] []
                            , text "Je gegevens worden na 24 uur volledig verwijderd uit de dagcrowd."
                            ]
                        ]
                    , div [ class "list__item__title" ]
                        [ div [] [ Html.map ParticipantMsg <| Edit.Edge.view (Just newParticipantFormatter) participant ]
                        , div []
                            [ label [ for "participant-email" ] [ text "E-mailadres:" ]
                            , input [ id "participant-email", placeholder "Voer e-mailadres in", class "form-control", value email, onInput EmailInput ] []
                            ]
                        , div []
                            [ input
                                [ type_ "checkbox"
                                , id "participant-email-in-profile"
                                , value "email"
                                , checked showContact
                                , onClick ShowContactToggle
                                ]
                                []
                            , label [ for "participant-email-in-profile" ]
                                [ text "Mijn e-mail mag zichtbaar zijn voor andere deelnemers" ]
                            ]
                        ]
                    , viewMaybe keywords
                        (\k -> p [] [ Html.map KeywordsMsg <| Edit.Edge.view Nothing k ])
                    , viewMaybe keywords
                        (\k ->
                            viewIf (Edit.Edge.validated k)
                                (\_ ->
                                    p [] [ button [ onClick SaveParticipant ] [ text "Voeg toe aan deze crowd" ] ]
                                )
                        )
                    ]
                ]
            ]
        ]


viewSelection : Maybe Node -> Html Msg
viewSelection selected =
    case selected of
        Nothing ->
            text "*Niets gehovered*"

        Just (TagNode tag) ->
            text ("Tag " ++ Translation.toString Translation.NL tag.title)

        Just (ParticipantNode participant) ->
            case participant of
                Crowd.Request.Participant user ->
                    text ("User " ++ Translation.toString Translation.NL user.title)

                Crowd.Request.Anonymous _ ->
                    text "Anoniem"

        _ ->
            text ""


nodeRadius : Node -> Model -> Float
nodeRadius node model =
    case node of
        TagNode tag ->
            let
                incomingConnections =
                    Graph.edges model.simulation.graph
                        |> List.filter (\edge -> edge.to == Id.toInt tag.id)
                        |> List.length

                totalNodes =
                    List.length (Graph.nodes model.simulation.graph)

                tagMultiplier =
                    1 + toFloat incomingConnections / toFloat totalNodes * 2
            in
            30 * tagMultiplier

        ParticipantNode participant ->
            case participant of
                Crowd.Request.Participant _ ->
                    50

                Crowd.Request.Anonymous _ ->
                    25

        CrowdNode ->
            50


viewGraph : Model -> Html Msg
viewGraph model =
    let
        nodePosition node =
            Point2d.pixels node.label.x node.label.y

        toBubble node =
            let
                adjIds =
                    Maybe.map
                        (\{ incoming, outgoing } ->
                            IntDict.keys
                                (IntDict.union incoming outgoing)
                        )
                        (Graph.get node.id model.simulation.graph)

                -- FIXME: Non-trivial performance cost.
                -- low-hanging fruit for optimization, by for example using the graph more directly and querying neighbours
                adjNodes =
                    Maybe.map
                        (List.filterMap
                            (\id -> Graph.get id model.simulation.graph)
                        )
                        adjIds

                isHighlighted =
                    shouldHighlight node model
                        || (Maybe.map (List.any (\ctx -> shouldHighlight ctx.node model)) adjNodes
                                |> Maybe.withDefault False
                           )
            in
            case node.label.value of
                TagNode tag ->
                    Just
                        { label = Just (Translation.toString Translation.NL tag.title)
                        , image = TextureStore.url Assets.tag
                        , position = nodePosition node
                        , radius = Quantity.unsafe (nodeRadius node.label.value model)
                        , highlight = isHighlighted
                        }

                ParticipantNode participant ->
                    case participant of
                        Crowd.Request.Participant user ->
                            Just
                                { label = Just (Translation.toString Translation.NL user.title)
                                , image =
                                    TextureStore.url (Maybe.withDefault Assets.person (Crowd.Request.getAvatar user))
                                , position = nodePosition node
                                , radius = Quantity.unsafe (nodeRadius node.label.value model)
                                , highlight = isHighlighted
                                }

                        Crowd.Request.Anonymous _ ->
                            Just
                                { label = Nothing
                                , image = TextureStore.url Assets.person
                                , position = nodePosition node
                                , radius = Quantity.unsafe (nodeRadius node.label.value model)
                                , highlight = isHighlighted
                                }

                CrowdNode ->
                    Nothing
    in
    Render.view
        [ Mouse.onMouseMove MouseMove
        , onClick MouseClick
        , onMouseDown MouseDown
        , onMouseUp MouseUp
        , Mouse.onTouchMove TouchMove
        , Mouse.onTouchStart TouchStart
        , Mouse.onTouchEnd TouchEnd
        ]
        model.canvasDimensions
        model.textureStore
        { nodes =
            List.filterMap toBubble <|
                Graph.nodes model.simulation.graph
        , connections =
            List.filterMap
                (\{ source, target, render } ->
                    if render then
                        Maybe.map2
                            (\ctxA ctxB ->
                                { a = nodePosition ctxA.node
                                , b = nodePosition ctxB.node
                                , highlight =
                                    shouldHighlight ctxA.node model
                                        || shouldHighlight ctxB.node model
                                }
                            )
                            (Graph.get source model.simulation.graph)
                            (Graph.get target model.simulation.graph)

                    else
                        Nothing
                )
                model.simulation.connections
        , camera = model.camera
        }


shouldHighlight : Graph.Node n -> Model -> Bool
shouldHighlight node model =
    case ( model.hoveredNode, model.selectedNode ) of
        ( Nothing, Nothing ) ->
            True

        ( Just ( hoverId, _ ), Just ( selectedId, _ ) ) ->
            node.id == selectedId || node.id == hoverId

        ( _, Just ( selectedId, _ ) ) ->
            node.id == selectedId

        ( Just ( hoverId, _ ), _ ) ->
            node.id == hoverId


viewQr : String -> Html Msg
viewQr message =
    div
        [ class "crowd__qr"
        ]
        (message
            |> QRCode.fromString
            |> Result.map
                (QRCode.toSvgWithoutQuietZone
                    [ SvgA.width "100px"
                    , SvgA.height "100px"
                    ]
                    >> List.singleton
                )
            |> Result.withDefault []
        )
