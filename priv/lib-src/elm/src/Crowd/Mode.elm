module Crowd.Mode exposing (..)

import Html exposing (Html, text)


type Mode
    = MeetupCrowd
    | DayCrowd


className : Mode -> String
className mode =
    case mode of
        MeetupCrowd ->
            "meetupcrowd"

        DayCrowd ->
            "daycrowd"


fromString : String -> Mode
fromString s =
    case s of
        "event" ->
            MeetupCrowd

        "daycrowdevent" ->
            DayCrowd

        _ ->
            MeetupCrowd


editable : Mode -> Bool -> Bool
editable crowdMode editableInMeetupMode =
    case crowdMode of
        MeetupCrowd ->
            editableInMeetupMode

        DayCrowd ->
            True


buttonText : Mode -> String
buttonText crowdMode =
    case crowdMode of
        MeetupCrowd ->
            "Voeg deelnemer toe aan de crowd"

        DayCrowd ->
            "Meld je aan"


allowAddExistingMembers : Mode -> Bool
allowAddExistingMembers crowdMode =
    case crowdMode of
        MeetupCrowd ->
            True

        DayCrowd ->
            False


addParticipantLabel : Mode -> String
addParticipantLabel crowdMode =
    case crowdMode of
        MeetupCrowd ->
            "Voeg een bestaande KennisCloud-gebruiker of tijdelijke deelnemer toe:"

        DayCrowd ->
            "Naam:"


keywordLabel : Mode -> String
keywordLabel crowdMode =
    case crowdMode of
        MeetupCrowd ->
            "In welke thema's is deze persoon geïnteresseerd? (minimaal 1)"

        DayCrowd ->
            "In welke thema's ben je geïnteresseerd? (minimaal 1)"


viewIfDayCrowd : Mode -> Html msg -> Html msg
viewIfDayCrowd mode view =
    case mode of
        MeetupCrowd ->
            text ""

        DayCrowd ->
            view
