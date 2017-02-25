module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Date exposing (..)
import Task exposing (..)
import Geolocation exposing (..)
import List exposing (..)
import Json.Decode exposing (string, Decoder, bool)
import Json.Decode.Pipeline exposing (..)
import Http


-- # Main


main : Program Never Model Msg
main =
    Html.program
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


init : ( Model, Cmd Msg )
init =
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
    | GetItinerary (Result Http.Error Itinerary)
    | InputSearch String
    | NoOp



-- # Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetInitialDate date ->
            { model | startTime = Just date } ! []

        GetInitialLocation (Ok location) ->
            { model | location = Just location } ! [ getItinerary ]

        GetInitialLocation (Err error) ->
            { model | location = Nothing } ! []

        GetItinerary (Ok itinerary) ->
            { model | itinerary = Just itinerary.list } ! []

        GetItinerary (Err error) ->
            model ! []

        InputSearch string ->
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
            div [ class "container" ]
                [ h1 [] [ text "I <3 Macklemore" ]
                , div [] [ text "Time: ", text (toString model.startTime) ]
                , div [] [ text "Location: ", text (toString model.location) ]
                , itineraryView itinerary
                ]

        Nothing ->
            div [ class "container" ]
                [ h1 [] [ text "I <3 Macklemore" ]
                , div [] [ text "Time: ", text (toString model.startTime) ]
                , div [] [ text "Location: ", text (toString model.location) ]
                , div [] [ text "Please turn on your JS stuff kthx" ]
                ]


type alias Itinerary =
    { list : List Place
    }


type alias Place =
    { title : String
    , eventTime : Date
    , location : String
    }


defaultDate : Date.Date
defaultDate =
    case Date.fromString "1970-01-01T00:00:00Z" of
        Ok date ->
            date

        Err _ ->
            Debug.crash "Invalid Date"


placeCard : Place -> Html Msg
placeCard place =
    div
        [ class "container" ]
        [ p [] [ text place.title ]
        , p [] [ text <| toString place.eventTime ]
        , p [] [ text place.location ]
        ]


itineraryView : List Place -> Html Msg
itineraryView places =
    div [] <|
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
        Http.get "http://localhost:3002/api/itinerary" decodeItinerary


decodeItinerary : Decoder Itinerary
decodeItinerary =
    decode Itinerary
        |> requiredAt [ "itinerary" ] (Json.Decode.list decodePlace)


decodePlace : Decoder Place
decodePlace =
    decode Place
        |> required "title" string
        |> required "eventTime" (Json.Decode.map (Result.withDefault defaultDate << Date.fromString) string)
        |> required "location" string
