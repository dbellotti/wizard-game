module Main exposing (..)

import Array exposing (Array)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Time exposing (Time, second)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { counter : Int
    , positions : Array String

    --, arrows:
    }


init : ( Model, Cmd Msg )
init =
    ( { counter = 0
      , positions = Array.fromList [ "wizard-forward", "wizard-right", "wizard-back", "wizard-left" ]
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Rotate Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Rotate newTime ->
            let
                newCounter =
                    if model.counter < 3 then
                        model.counter + 1
                    else
                        0
            in
            ( { model | counter = newCounter }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Rotate



-- VIEW


view : Model -> Html msg
view model =
    div []
        [ text ("Counter: " ++ toString model.counter)
        , div [ class "wizard", class (getWizardClass model) ] []
        ]



-- HELPER


getWizardClass : Model -> String
getWizardClass model =
    case Array.get model.counter model.positions of
        Just className ->
            className

        Nothing ->
            ""
