port module Main exposing (..)

import Html exposing (..)
import Navigation
import Html.Attributes
    exposing
        ( class
        , href
        , id
        , type_
        , placeholder
        , value
        , step
        , classList
        , disabled
        )
import Html.Events exposing (onClick, onInput)
import Task exposing (..)
import Geolocation exposing (..)
import List exposing (..)
import Json.Encode
import Json.Decode exposing (string, Decoder, bool, int, float)
import Json.Decode.Pipeline exposing (..)
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
    , itinerary : Maybe (Array Place)
    , placeOpen : Maybe Int
    , selectedActivities : List String
    }


defaultModel : Model
defaultModel =
    Model
        Nothing
        Nothing
        Nothing
        []


defaultActivityList : List String
defaultActivityList =
    [ "Dinner"
    , "Movie"
    , "Park"
    , "Bar"
    , "Club"
    , "Sports"
    ]


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    defaultModel
        ! [ Cmd.batch
                [ Task.attempt GetInitialLocation Geolocation.now
                ]
          ]



-- # Messages


type Msg
    = GetInitialLocation (Result Error Location)
    | GetItinerary (Result Http.Error (List Place))
    | UrlChange Navigation.Location
    | TogglePlaceDescription Int
    | AddActivity String
    | NoOp



-- # Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- there is an assumption here that we will have the date before the location
        GetInitialLocation (Ok location) ->
            let
                newModel =
                    { model | location = Just location }
            in
                newModel ! []

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

        AddActivity activity ->
            let
                activities =
                    List.append model.selectedActivities [ activity ]
            in
                { model | selectedActivities = activities } ! []

        UrlChange location ->
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


noBubble : Html.Events.Options
noBubble =
    { stopPropagation = True
    , preventDefault = True
    }


onClickNoBubble : msg -> Html.Attribute msg
onClickNoBubble msg =
    Html.Events.onWithOptions "click" noBubble (Json.Decode.succeed msg)


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
            div
                []
                [ text "Loading..." ]


type alias Itinerary =
    { list : List Place
    }


type alias Place =
    { title : String
    , description : String
    , distance : Int
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
        currLocation =
            case model.location of
                Just location ->
                    (toString location.longitude) ++ ", " ++ (toString location.latitude)

                Nothing ->
                    Debug.crash "location isn't loaded"

        body =
            Http.jsonBody (encodeItineraryRequest (ItineraryRequest currLocation model.selectedActivities))
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
        |> required "location" decodePlaceLocation


type alias ItineraryRequest =
    { start_point : String
    , activities : List String
    }


encodeItineraryRequest : ItineraryRequest -> Json.Encode.Value
encodeItineraryRequest record =
    Json.Encode.object
        [ ( "start_point", Json.Encode.string <| record.start_point )
        , ( "activies", Json.Encode.list <| List.map Json.Encode.string record.activities )
        ]
