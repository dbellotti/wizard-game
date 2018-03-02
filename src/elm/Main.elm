module Main exposing (..)

import AnimationFrame
import Array exposing (Array)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Keyboard
import Time exposing (Time, inMilliseconds)
import Tuple exposing (first, second)


type alias Model =
    { keyCode : Int
    , wizard : Wizard
    }


type alias Wizard =
    { orientation : String
    , posX : Int
    , posY : Int
    , speed : Int
    , isMovingXY : (Bool, Bool)
    }


-- INIT


init : ( Model, Cmd Msg )
init =
    ( { keyCode = 0
      , wizard =
          { orientation = "wizard-left"
          , posX = 0
          , posY = 0
          , speed = 10
          , isMovingXY = (False, False)
          }
      }
    , Cmd.none
    )


-- UPDATE


type Msg
    = KeyDownMsg Keyboard.KeyCode
    | KeyUpMsg Keyboard.KeyCode
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            let
                wizard =
                    model.wizard

                timeDiff =
                    time |> inMilliseconds |> round

                newWizard =
                    case (wizard.orientation, wizard.isMovingXY) of
                        ("wizard-left",    (True, False)) -> { wizard | posX = wizard.posX - timeDiff }
                        ("wizard-right",   (True, False)) -> { wizard | posX = wizard.posX + timeDiff }
                        ("wizard-back",    (False, True)) -> { wizard | posY = wizard.posY + timeDiff }
                        ("wizard-forward", (False, True)) -> { wizard | posY = wizard.posY - timeDiff }
                        _                                 -> wizard

            in
                ( { model | wizard = newWizard }, Cmd.none )


        KeyDownMsg code ->
            let
                wizard =
                    model.wizard

                newWizard =
                    case code of
                        37 -> { wizard | isMovingXY = (True, False), orientation = "wizard-left" }
                        39 -> { wizard | isMovingXY = (True, False), orientation = "wizard-right" }
                        38 -> { wizard | isMovingXY = (False, True), orientation = "wizard-back" }
                        40 -> { wizard | isMovingXY = (False, True), orientation = "wizard-forward" }
                        _  -> wizard

            in
                ( { model | wizard = newWizard , keyCode = code }, Cmd.none )


        KeyUpMsg code ->
            let
                wizard =
                    model.wizard

                newWizard =
                    case (code, wizard.orientation) of
                        (37, "wizard-left")    -> { wizard | isMovingXY = (False, second wizard.isMovingXY) }
                        (39, "wizard-right")   -> { wizard | isMovingXY = (False, second wizard.isMovingXY) }
                        (38, "wizard-back")    -> { wizard | isMovingXY = (first wizard.isMovingXY, False) }
                        (40, "wizard-forward") -> { wizard | isMovingXY = (first wizard.isMovingXY, False) }
                        _                      -> wizard

            in
                ( { model | wizard = newWizard }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Keyboard.downs KeyDownMsg
        , Keyboard.ups KeyUpMsg
        ]



-- VIEW


view : Model -> Html msg
view model =
    div []
        [ text ("KeyCode: " ++ toString model.keyCode)
        , div
            [ class "wizard"
            , class model.wizard.orientation
            , style
                [ ( "left", toString model.wizard.posX ++ "px" )
                , ( "bottom", toString model.wizard.posY ++ "px" )
                ]
            ]
            []
        ]


-- MAIN


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

