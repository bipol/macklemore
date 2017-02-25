module Main exposing (..)

import Html exposing (..)
import Navigation
import Html.Attributes exposing (class, href)
import Date exposing (..)
import Task exposing (..)
import Geolocation exposing (..)
import List exposing (..)
import Json.Decode exposing (string, Decoder, bool, int, float)
import Json.Decode.Pipeline exposing (..)
import Http


-- # Main


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- # Model


type alias Model =
    { location : Maybe Location
    , startTime : Maybe Date
    , endTime : Maybe Date
    , itinerary : Maybe (List Place)
    }


defaultModel : Model
defaultModel =
    Model
        Nothing
        Nothing
        Nothing
        Nothing


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    defaultModel
        ! [ Cmd.batch
                [ Task.perform GetInitialDate Date.now
                , Task.attempt GetInitialLocation Geolocation.now
                ]
          ]



-- # Messages


type Msg
    = GetInitialDate Date
    | GetInitialLocation (Result Error Location)
    | GetItinerary (Result Http.Error (List Place))
    | SetEndDate String
    | InputSearch String
    | UrlChange Navigation.Location
    | NoOp



-- # Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetInitialDate date ->
            { model | startTime = Just date } ! []

        -- there is an assumption here that we will have the date before the location
        GetInitialLocation (Ok location) ->
            { model | location = Just location } ! [ getItinerary ]

        GetInitialLocation (Err error) ->
            { model | location = Nothing } ! []

        GetItinerary (Ok itinerary) ->
            { model | itinerary = Just itinerary } ! []

        GetItinerary (Err error) ->
            model ! []

        InputSearch string ->
            model ! []

        UrlChange location ->
            model ! []

        SetEndDate time ->
            model ! []

        NoOp ->
            model ! []



-- # View


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    case model.itinerary of
        Just itinerary ->
            div []
                [ itineraryView itinerary
                ]

        Nothing ->
            div []
                [ div [] [ text "Loading..." ]
                ]


type alias Itinerary =
    { list : List Place
    }


type alias Place =
    { title : String
    , description : String
    , distance : Int
    , event_time : Date
    , location : PlaceLocation
    }


type alias PlaceLocation =
    { address : String
    , lat : Int
    , long : Int
    }


defaultDate : Date.Date
defaultDate =
    case Date.fromString "1970-01-01T00:00:00Z" of
        Ok date ->
            date

        Err _ ->
            Debug.crash "Invalid Date"


encodeGoogleUrl : PlaceLocation -> String
encodeGoogleUrl placeLocation =
    "https://maps.google.com/?q=@"
        ++ toString placeLocation.lat
        ++ ", "
        ++ toString placeLocation.long


placeCard : Place -> Html Msg
placeCard place =
    div
        [ class "event" ]
        [ div
            [ class "event__duration" ]
            [ div
                [ class "event__duration-value" ]
                [ text "2" ]
            , div
                [ class "event__duration-unit" ]
                [ text "HR" ]
            ]
        , div
            [ class "event__meta" ]
            [ div
                [ class "event__title" ]
                [ text place.title ]
            , div
                [ class "event__location" ]
                [ text place.location.address ]
            , div
                [ class "event__time-location" ]
                [ div
                    [ class "event__time-block" ]
                    [ text <| toString place.event_time ]
                , div
                    [ class "event__location-link" ]
                    [ a [ href <| encodeGoogleUrl place.location ] [ text "show on map" ] ]
                ]
            ]
        , div
            [ class "event__distance" ]
            [ div
                [ class "event__distance-value" ]
                [ text <| toString place.distance ]
            , div
                [ class "event__distance-unit" ]
                [ text "Mi" ]
            ]
        ]


itineraryView : List Place -> Html Msg
itineraryView places =
    div [ class "list" ] <|
        List.map
            placeCard
            places



-- Rest Portion


urlBuilder : List String -> String
urlBuilder parts =
    List.foldr (++) "" <| List.intersperse "/" parts


getItinerary : Cmd Msg
getItinerary =
    Http.send GetItinerary <|
        Http.get "http://localhost:3002/api/itinerary" (Json.Decode.list decodePlace)


decodePlaceLocation : Decoder PlaceLocation
decodePlaceLocation =
    decode PlaceLocation
        |> required "address" string
        |> required "lat" int
        |> required "long" int


decodePlace : Decoder Place
decodePlace =
    decode Place
        |> required "title" string
        |> required "description" string
        |> required "distance" int
        |> required "event_time" (Json.Decode.map Date.fromTime float)
        |> required "location" decodePlaceLocation
