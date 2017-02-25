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
        , attribute
        , src
        )
import Tuple exposing (..)
import Html.Events exposing (onClick, onInput, onMouseDown)
import Geolocation exposing (..)
import List exposing (..)
import Json.Encode
import Json.Decode exposing (string, Decoder, bool, int, float, list)
import Json.Decode.Pipeline exposing (..)
import Http
import Array exposing (Array)
import Html.Keyed


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


port createCarousel : Int -> Cmd msg



-- # Model


type alias Model =
    { location : Maybe Location
    , itinerary : Maybe (Array Place)
    , placeOpen : Maybe Int
    , selectedActivities : List String
    , isAjax : Bool
    }


defaultModel : Model
defaultModel =
    Model
        Nothing
        Nothing
        Nothing
        []
        False


defaultActivityList : List ( String, String )
defaultActivityList =
    [ ( "Dinner", "fa-film" )
    , ( "Movie", "fa-film" )
    , ( "Park", "fa-cc-diners-club" )
    , ( "Bar", "fa-beer" )
    , ( "Club", "fa-cc-diners-club" )
    , ( "Sports", "fa-soccer-ball-o" )
    ]


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    defaultModel
        ! [ Cmd.batch
                [ createCarousel 1
                ]
          ]



-- # Messages


type Msg
    = GetInitialLocation (Result Error Location)
    | GetItinerary (Result Http.Error (List Place))
    | UrlChange Navigation.Location
    | TogglePlaceDescription Int
    | AddActivity String
    | SubmitActivity
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
                    if List.member activity model.selectedActivities then
                        List.filter (\e -> not <| e == activity) model.selectedActivities
                    else
                        List.append model.selectedActivities [ activity ]
            in
                { model | selectedActivities = activities } ! []

        UrlChange location ->
            model ! []

        SubmitActivity ->
            { model | isAjax = True } ! [ getItinerary model ]

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


getLineClass : List String -> String
getLineClass lines =
    case List.head lines of
        Just aLine ->
            case aLine of
                "RED" ->
                    "event--red-line"

                "GOLD" ->
                    "event--gold-line"

                _ ->
                    "event--gold-line"

        Nothing ->
            "event--gold-line"


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
    div
        []
        [ carouselView model ]


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
    , line : List String
    }


encodeGoogleUrl : PlaceLocation -> String
encodeGoogleUrl placeLocation =
    "https://maps.google.com/?q=@"
        ++ toString placeLocation.lat
        ++ ", "
        ++ toString placeLocation.long


placeCard : Maybe Int -> Int -> Place -> ( String, Html Msg )
placeCard currOpen idx place =
    let
        lineClass =
            getLineClass place.location.line

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
        ( toString idx
        , div
            [ classList [ ( "event", True ), ( lineClass, True ) ]
            ]
            [ div
                [ class "event__duration" ]
                [ div
                    [ class "event__duration-value" ]
                    [ text "2" ]
                ]
            , div
                [ class "event__meta" ]
                [ div
                    [ class "event__title" ]
                    [ text place.title
                    , i [ classList [ ( "fa", True ), ( "fa-exchange", True ) ] ] []
                    ]
                , div
                    [ class "event__location" ]
                    [ text place.location.address ]
                , div
                    [ class "event__time-location" ]
                    [ div [ class "event__time-block" ] [ text "1:00pm-3:00pm" ]
                    , div [ class "event__location-link" ]
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
                    [ text "mi" ]
                ]
            , descBox
            ]
        )


itineraryView : Array Place -> Maybe Int -> Html Msg
itineraryView places currSel =
    Html.Keyed.node "div" [ class "list" ] <|
        Array.toList <|
            Array.indexedMap
                (placeCard currSel)
                places


activityButton : List String -> ( String, String ) -> ( String, Html Msg )
activityButton currSelected ( activity, icon ) =
    let
        isSelected =
            List.member activity currSelected

        idx =
            Tuple.first <| Maybe.withDefault ( 0, "" ) <| List.head <| List.filter (\x -> Tuple.second x == activity) <| Array.toIndexedList <| Array.fromList currSelected
    in
        ( activity
        , button
            [ classList [ ( "activities__btn", True ), ( "selected", isSelected ) ]
            , attribute "data-bw" activity
            , onClick <| AddActivity activity
            ]
            [ div [ class "activities__order" ] [ text <| toString idx ]
            , span [] [ text activity ]
            , i [ classList [ ( "fa", True ), ( icon, True ) ] ] []
            ]
        )


carouselView : Model -> Html Msg
carouselView model =
    let
        view =
            case model.itinerary of
                Just itinerary ->
                    let
                        goHunting =
                            List.isEmpty model.selectedActivities
                    in
                        div [ class "carousel__container" ] <|
                            [ Html.Keyed.node "div"
                                [ classList [ ( "carousel", True ), ( "carousel--second-slide", True ) ] ]
                                [ ( "first-node"
                                  , div
                                        [ class "carousel__slide" ]
                                        [ div [ class "carousel__slide-title" ] [ text "What are you getting into?" ]
                                        , (Html.Keyed.node "div" [ class "activities" ] <| List.map (activityButton model.selectedActivities) defaultActivityList)
                                        , button
                                            [ class "carousel__next-btn"
                                            , onClickNoBubble SubmitActivity
                                            , disabled goHunting
                                            ]
                                            [ span [] [ text "Let's Go Hunting" ]
                                            , i [ classList [ ( "fa", True ), ( "fa-chevron-right", True ) ] ]
                                                []
                                            ]
                                        ]
                                  )
                                , ( "second-node"
                                  , div [ class "carousel__slide" ] [ itineraryView itinerary model.placeOpen ]
                                  )
                                ]
                            ]

                Nothing ->
                    let
                        goHunting =
                            List.isEmpty model.selectedActivities

                        moveOver =
                            if model.isAjax then
                                ( "carousel--second-slide", True )
                            else
                                ( "", False )
                    in
                        div [ class "carousel__container" ] <|
                            [ Html.Keyed.node "div"
                                [ classList [ ( "carousel", True ), moveOver ] ]
                                [ ( "first-node"
                                  , div
                                        [ class "carousel__slide" ]
                                        [ div [ class "carousel__slide-title" ] [ text "What are you getting into?" ]
                                        , (Html.Keyed.node "div" [ class "activities" ] <| List.map (activityButton model.selectedActivities) defaultActivityList)
                                        , button
                                            [ class "carousel__next-btn"
                                            , onClickNoBubble SubmitActivity
                                            , disabled goHunting
                                            ]
                                            [ span [] [ text "Let's Go Hunting" ]
                                            , i [ classList [ ( "fa", True ), ( "fa-chevron-right", True ) ] ]
                                                []
                                            ]
                                        ]
                                  )
                                , ( "second-node"
                                  , div [ class "carousel__slide" ]
                                        [ div
                                            [ class "carousel__spinner" ]
                                            [ img
                                                [ src "img/loading.gif" ]
                                                []
                                            ]
                                        ]
                                  )
                                ]
                            ]
    in
        view



-- Rest Portion


urlBuilder : List String -> String
urlBuilder parts =
    List.foldr (++) "" <| List.intersperse "/" parts


getItinerary : Model -> Cmd Msg
getItinerary model =
    let
        body =
            Http.jsonBody (encodeItineraryRequest (ItineraryRequest "DONT" model.selectedActivities))
    in
        Http.send GetItinerary <|
            Http.post "https://malone-api.herokuapp.com/api/itinerary" body (Json.Decode.list decodePlace)


decodePlaceLocation : Decoder PlaceLocation
decodePlaceLocation =
    decode PlaceLocation
        |> required "address" string
        |> required "lat" float
        |> required "long" float
        |> required "line" (list string)


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
        [ ( "start_point", Json.Encode.string <| "848 Spring St. Atlanta, GA" )
        , ( "activities", Json.Encode.list <| List.map Json.Encode.string record.activities )
        ]
