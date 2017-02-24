module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Date exposing (..)
import Task exposing (..)
import Geolocation exposing (..)


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
    }


defaultModel : Model
defaultModel =
    Model
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
    | InputSearch String
    | NoOp



-- # Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetInitialDate date ->
            { model | startTime = Just date } ! []

        GetInitialLocation (Ok location) ->
            { model | location = Just location } ! []

        GetInitialLocation (Err location) ->
            { model | location = Nothing } ! []

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
    div [ class "container" ]
        [ h1 [] [ text "I <3 Macklemore" ]
        , div [] [ text "Time: ", text (toString model.startTime) ]
        , div [] [ text "Location: ", text (toString model.location) ]
        ]


type alias Place =
    { title : String
    , eventTime : Date
    , location : String
    }


placeCard : Place -> Html Msg
placeCard place =
    div
        []
        []


itinerary : List Place -> Html Msg
itinerary places =
    div []
        []
