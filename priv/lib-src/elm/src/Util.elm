module Util exposing
    ( Now
    , decodeDefaultTranslation
    , decodeTimezone
    , decodeTranslation
    , decodeTranslationOrDefault
    , errorToString
    , formatTime
    , millisToNow
    , nowToPosix
    , timeDistanceInWords
    )

import DateFormat
import DateFormat.Language as DateFormat
import DateFormat.Relative as DateFormat
import Ginger.Id as Id exposing (ResourceId)
import Ginger.Resource as Resource exposing (Edges, ResourceWith)
import Ginger.Translation as Translation exposing (Translation)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Iso8601
import Json.Decode as D
import String.Interpolate as String
import Time
import Url.Builder



-- NOW


type Now
    = Now Time.Posix


millisToNow : Int -> Now
millisToNow =
    Now << Time.millisToPosix


nowToPosix : Now -> Time.Posix
nowToPosix (Now now) =
    now



-- FORMAT


formatTime : Now -> Time.Posix -> Time.Zone -> Html msg
formatTime (Now now) dateTime zone =
    let
        isoTime =
            Iso8601.fromTime dateTime

        posixToHours x =
            toFloat (Time.posixToMillis x) / 1000 / 60 / 60
    in
    time [ datetime isoTime, title isoTime ] <|
        if posixToHours now - posixToHours dateTime > 12 then
            [ text (dateToString zone dateTime) ]

        else
            [ text (timeDistanceInWords now dateTime) ]


dateToString : Time.Zone -> Time.Posix -> String
dateToString =
    DateFormat.formatWithLanguage DateFormat.dutch
        [ DateFormat.dayOfMonthFixed
        , DateFormat.text "-"
        , DateFormat.monthFixed
        , DateFormat.text "-"
        , DateFormat.yearNumber
        ]


timeDistanceInWords : Time.Posix -> Time.Posix -> String
timeDistanceInWords =
    DateFormat.relativeTimeWithOptions nlRelativeOptions


nlRelativeOptions : DateFormat.RelativeTimeOptions
nlRelativeOptions =
    { someSecondsAgo = formatter ( "seconde", "seconden" )
    , someMinutesAgo = formatter ( "minuut", "minuten" )
    , someHoursAgo = formatter ( "uur", "uur" )
    , someDaysAgo = formatter ( "dag", "dagen" )
    , someMonthsAgo = formatter ( "maand", "maanden" )
    , someYearsAgo = formatter ( "jaar", "jaar" )
    , rightNow = "nu"
    , inSomeSeconds = formatterFuture ( "seconde", "seconden" )
    , inSomeMinutes = formatterFuture ( "minuut", "minuten" )
    , inSomeHours = formatterFuture ( "uur", "uur" )
    , inSomeDays = formatterFuture ( "dag", "dagen" )
    , inSomeMonths = formatterFuture ( "maand", "maanden" )
    , inSomeYears = formatterFuture ( "jaar", "jaar" )
    }


formatter : ( String, String ) -> Int -> String
formatter ( singular, plural ) value =
    String.interpolate "{0} {1} geleden"
        [ String.fromInt value
        , if value == 1 then
            singular

          else
            plural
        ]


formatterFuture : ( String, String ) -> Int -> String
formatterFuture ( singular, plural ) value =
    String.interpolate "over {0} {1}"
        [ String.fromInt value
        , if value == 1 then
            singular

          else
            plural
        ]


decodeTranslationOrDefault : D.Decoder Translation
decodeTranslationOrDefault =
    -- Decode a translation record either from a translation object or from a
    -- string using the default languages EN and NL
    D.oneOf [ decodeDefaultTranslation [ Translation.EN, Translation.NL ], decodeTranslation ]


decodeDefaultTranslation : List Translation.Language -> D.Decoder Translation
decodeDefaultTranslation languages =
    -- Decode a string into a translation record for the given languages
    D.string
        |> D.map (\val -> Translation.fromList (List.map (\lang -> ( lang, val )) languages))


decodeTranslation : D.Decoder Translation
decodeTranslation =
    -- 'Ginger.Translation.fromJson' is not compatible with the JSON representation
    -- of the 'trans' record in Zotonic 1.x, because it's not accounting for the "tr" field.
    D.field "tr" Translation.fromJson


decodeTimezone : D.Decoder Time.Zone
decodeTimezone =
    D.string
        |> D.map
            (\s ->
                case List.filterMap String.toInt (String.split ":" s) of
                    hours :: minutes :: _ ->
                        Time.customZone (hours * 60 + minutes) []

                    hours :: _ ->
                        Time.customZone (hours * 60) []

                    _ ->
                        Time.utc
            )


errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "The URL " ++ url ++ " was invalid"

        Http.Timeout ->
            "Unable to reach the server, try again"

        Http.NetworkError ->
            "Unable to reach the server, check your network connection"

        Http.BadStatus 500 ->
            "The server had a problem, try again later"

        Http.BadStatus 400 ->
            "Verify your information and try again"

        Http.BadStatus _ ->
            "Unknown error"

        Http.BadBody errorMessage ->
            errorMessage
