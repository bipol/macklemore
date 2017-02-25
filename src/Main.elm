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
    , sliderTime : Float
    }


defaultModel : Model
defaultModel =
    Model
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        0.5


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
    | InputSearch String
    | UrlChange Navigation.Location
    | TogglePlaceDescription Int
    | ChangeSlider Float
    | SubmitEndTime
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

        InputSearch string ->
            model ! []

        UrlChange location ->
            model ! []

        ChangeSlider interval ->
            let
                _ =
                    Debug.log "interval" interval
            in
                if not <| interval == model.sliderTime then
                    { model | sliderTime = interval } ! []
                else
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

        SubmitEndTime ->
            case model.startTime of
                Just startTime ->
                    { model | endTime = Just <| Date.fromTime <| (Date.toTime startTime) + (360000 * model.sliderTime) } ! [ getItinerary model ]

                Nothing ->
                    model ! []

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
            let
                submitBtn =
                    case model.location of
                        Just _ ->
                            button
                                [ class "landing__btn", onClickNoBubble SubmitEndTime ]
                                [ text "Submit" ]

                        Nothing ->
                            button
                                [ class "landing__btn", disabled True ]
                                [ text "Loading..." ]
            in
                div
                    [ class "landing" ]
                    [ div
                        [ class "landing__title" ]
                        [ text "Hackavelli" ]
                    , div
                        [ class "landing__form" ]
                        [ div [ class "landing__ttk" ]
                            [ div [ class "landing__time-label" ]
                                [ text "Time to kill:"
                                , input
                                    [ class "landing__time"
                                    , type_ "text"
                                    , placeholder "Hours"
                                    , value <| toString model.sliderTime
                                    , onInput (ChangeSlider << Result.withDefault 0.5 << String.toFloat)
                                    ]
                                    []
                                ]
                            ]
                        , div [ class "slider__container" ]
                            [ div
                                [ class "slider__rotate-container" ]
                                [ div
                                    [ class "slider__input-container" ]
                                    [ div
                                        [ class "slider--track-fill" ]
                                        []
                                    , input
                                        [ class "slider"
                                        , type_ "range"
                                        , Html.Attributes.min "0.5"
                                        , Html.Attributes.max "8"
                                        , step "0.5"
                                        , value <| toString model.sliderTime
                                        , onInput (ChangeSlider << Result.withDefault 0.5 << String.toFloat)
                                        ]
                                        []
                                    ]
                                , div
                                    [ class "slider__key" ]
                                    [ div
                                        [ class "slider__key-label" ]
                                        [ div [] []
                                        , span [] [ text "1/2 hour" ]
                                        ]
                                    , div
                                        [ classList [ ( "slider__key-label", True ), ( "slider--two-hr", True ) ] ]
                                        [ div [] []
                                        , span [] [ text "2 hours" ]
                                        ]
                                    , div
                                        [ classList [ ( "slider__key-label", True ), ( "slider--four-hr", True ) ] ]
                                        [ div [] []
                                        , span [] [ text "4 hours" ]
                                        ]
                                    , div
                                        [ classList [ ( "slider__key-label", True ), ( "slider--six-hr", True ) ] ]
                                        [ div [] []
                                        , span [] [ text "6 hours" ]
                                        ]
                                    , div
                                        [ classList [ ( "slider__key-label", True ), ( "slider--eight-hr", True ) ] ]
                                        [ div [] []
                                        , span [] [ text "8 hours" ]
                                        ]
                                    ]
                                ]
                            ]
                        , submitBtn
                        ]
                    , div
                        [ class "landing__description" ]
                        []
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
