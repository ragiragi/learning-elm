import Html exposing (..)
import Html.App as App
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)


timeZone = 9

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model =
  { time : Time
  , paused : Bool
  }

init : (Model, Cmd Msg)
init =
  (Model 0 False, Cmd.none)


-- UPDATE

type Msg
  = Tick Time
  | TogglePause

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ({ model | time = newTime }, Cmd.none)

    TogglePause ->
      ({ model | paused = not model.paused }, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  if model.paused then
    Sub.none
  else
    Time.every second Tick



-- VIEW

view : Model -> Html Msg
view model =
  let
    pauseButtonText =
      if model.paused then
        "Resume"
      else
        "Pause"
  in
    div []
      [ clock model
      , button [ onClick TogglePause ] [ Html.text pauseButtonText ]
      ]


clock : Model -> Html Msg
clock model =
  let
    secondAngle =
      degrees (toFloat (((floor (Time.inSeconds model.time)) % 60) * 6))

    minuteAngle =
      degrees (toFloat (((floor (Time.inMinutes model.time)) % 60) * 6))

    hourAngle =
      degrees
        (toFloat ((((floor (Time.inHours model.time)) + timeZone) % 12) * 30))
  in
    Svg.svg
      [ viewBox "0 0 100 100", width "300px" ]
      [ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
      , clockHand hourAngle (HandOptions 20 "#023963" "1.5px")
      , clockHand minuteAngle (HandOptions 30 "#023963" "0.7px")
      , clockHand secondAngle (HandOptions 40 "#ff0000" "0.3px")
      ]

type alias HandOptions =
  { length : Float
  , color : String
  , strokeWidth : String
  }

clockHand : Float -> HandOptions -> Html Msg
clockHand angle options =
  let
    handX =
      toString (50 + options.length * cos (angle - degrees 90))

    handY =
      toString (50 + options.length * sin (angle - degrees 90))

  in
    line [ x1 "50", y1 "50"
         , x2 handX, y2 handY
         , stroke options.color
         , strokeWidth options.strokeWidth ]
         []
