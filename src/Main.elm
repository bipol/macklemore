port module Main exposing (..)

import Html exposing (..)
import Navigation
import Html.Attributes exposing (class, href, id)
import Html.Events exposing (onClick)
import Date exposing (..)
import Task exposing (..)
import Geolocation exposing (..)
import List exposing (..)
import Json.Encode
import Json.Decode exposing (string, Decoder, bool, int, float)
import Json.Decode.Pipeline exposing (..)
import Time exposing (Time)
import Http
import Array exposing (Array)


-- # Main


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


port createMap : ( Float, Float, Int ) -> Cmd msg


port destroyMap : Int -> Cmd msg



-- # Model


type alias Model =
    { location : Maybe Location
    , startTime : Maybe Date
    , endTime : Maybe Date
    , itinerary : Maybe (Array Place)
    , placeOpen : Maybe Int
    }


defaultModel : Model
defaultModel =
    Model
        Nothing
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
    | TogglePlaceDescription Int
    | NoOp



-- # Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetInitialDate date ->
            { model | startTime = Just date } ! []

        -- there is an assumption here that we will have the date before the location
        GetInitialLocation (Ok location) ->
            let
                newModel =
                    { model | location = Just location }
            in
                newModel ! [ getItinerary newModel ]

        GetInitialLocation (Err error) ->
            { model | location = Nothing } ! []

        GetItinerary (Ok itinerary) ->
            { model | itinerary = Just <| Array.fromList itinerary } ! []

        GetItinerary (Err error) ->
            let
                stuff =
                    Debug.log "STUFF" error
            in
                model ! []

        InputSearch string ->
            model ! []

        UrlChange location ->
            model ! []

        SetEndDate time ->
            model ! []

        TogglePlaceDescription idx ->
            case model.placeOpen of
                Just placeOpen ->
                    if idx == placeOpen then
                        { model | placeOpen = Nothing } ! [ destroyMap idx ]
                    else
                        { model | placeOpen = Just idx } ! [ Cmd.batch [ sendMapCmd idx model.location, destroyMap placeOpen ] ]

                Nothing ->
                    { model | placeOpen = Just idx } ! [ sendMapCmd idx model.location ]

        NoOp ->
            model ! []


sendMapCmd : Int -> Maybe Location -> Cmd Msg
sendMapCmd idx location =
    case location of
        Just location ->
            createMap ( location.latitude, location.longitude, idx )

        Nothing ->
            Cmd.none



-- # View


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    case model.itinerary of
        Just itinerary ->
            div []
                [ itineraryView itinerary model.placeOpen
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
    , lat : Float
    , long : Float
    }


encodeGoogleUrl : PlaceLocation -> String
encodeGoogleUrl placeLocation =
    "https://maps.google.com/?q=@"
        ++ toString placeLocation.lat
        ++ ", "
        ++ toString placeLocation.long


placeCard : Maybe Int -> Int -> Place -> Html Msg
placeCard currOpen idx place =
    let
        descBox =
            div [ id <| "map" ++ toString idx ] []

        --            case currOpen of
        --                        div [ id <| "map" ++ idx ] []
        --                Just curr ->
        --                    if (curr == idx) then
        --                        div [ id <| "map" ++ idx ] []
        --                    else
        --                        span [] []
        --
        --                Nothing ->
        --                    span [] []
    in
        div
            [ class "event"
            , onClick <| TogglePlaceDescription idx
            ]
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
            , descBox
            ]


itineraryView : Array Place -> Maybe Int -> Html Msg
itineraryView places currSel =
    div [ class "list" ] <|
        Array.toList <|
            Array.indexedMap
                (placeCard currSel)
                places



-- Rest Portion


urlBuilder : List String -> String
urlBuilder parts =
    List.foldr (++) "" <| List.intersperse "/" parts


getItinerary : Model -> Cmd Msg
getItinerary model =
    let
        currTime =
            case model.startTime of
                Just startTime ->
                    Date.toTime startTime

                Nothing ->
                    Debug.crash "location isn't loaded"

        currLocation =
            case model.location of
                Just location ->
                    (toString location.longitude) ++ ", " ++ (toString location.latitude)

                Nothing ->
                    Debug.crash "location isn't loaded"

        body =
            Http.jsonBody (encodeItineraryRequest (ItineraryRequest currTime currTime currLocation))
    in
        Http.send GetItinerary <|
            Http.post "https://malone-api.herokuapp.com/api/itinerary" body (Json.Decode.list decodePlace)


decodePlaceLocation : Decoder PlaceLocation
decodePlaceLocation =
    decode PlaceLocation
        |> required "address" string
        |> required "lat" float
        |> required "long" float


decodePlace : Decoder Place
decodePlace =
    decode Place
        |> required "title" string
        |> required "description" string
        |> required "distance" int
        |> required "event_time" (Json.Decode.map (Date.fromTime << (*) 1000) float)
        |> required "location" decodePlaceLocation


type alias ItineraryRequest =
    { start_time : Time
    , end_time : Time
    , start_point : String
    }


encodeItineraryRequest : ItineraryRequest -> Json.Encode.Value
encodeItineraryRequest record =
    Json.Encode.object
        [ ( "start_time", Json.Encode.float <| record.start_time )
        , ( "end_time", Json.Encode.float <| record.end_time )
        , ( "start_point", Json.Encode.string <| record.start_point )
        ]
