import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events as Events exposing (on, onInput)
import Json.Decode as Json

import Gif

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model =
  { topic : String
  , widgets : List GifWidget
  , uid : Int
  }

type alias GifWidget =
  { id : Int
  , model : Gif.Model
  }

init : (Model, Cmd Msg)
init =
  (Model "" [] 0, Cmd.none)


-- UPDATE

type Msg
  = SubMsg Int Gif.Msg
  | UpdateTopic String
  | Create


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateTopic newTopic ->
      ( { model | topic = newTopic }
      , Cmd.none
      )

    Create ->
      let
        id = model.uid

        (newGifModel, widgetCmds) = Gif.init model.topic

        newWidget = GifWidget id newGifModel

        newWidgets = model.widgets ++ [newWidget]

      in
        ( Model "" newWidgets (id + 1)
        , Cmd.map (SubMsg id) widgetCmds
        )

    SubMsg id subMsg ->
      let
        (newWidgets, widgetCmds) =
          List.unzip (List.map (updateWidget id subMsg) model.widgets)
      in
        ( { model | widgets = newWidgets }
        , Cmd.batch widgetCmds
        )

updateWidget : Int -> Gif.Msg -> GifWidget -> (GifWidget, Cmd Msg)
updateWidget id msg widget =
  if widget.id /= id then
    (widget, Cmd.none)

  else
    let
      (newGifModel, widgetCmds) = Gif.update msg widget.model
    in
      ( GifWidget id newGifModel
      , Cmd.map (SubMsg id) widgetCmds
      )


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input
        [ placeholder "What kind of gifs do you want?"
        , value model.topic
        , onEnter Create
        , onInput UpdateTopic
        , inputStyle
        ]
        []
    , div
      [ style [ ("display", "flex"), ("flex-wrap", "wrap") ]
      ]
      (List.map viewWidget model.widgets)
    ]

viewWidget : GifWidget -> Html Msg
viewWidget {id, model} =
  App.map (SubMsg id) (Gif.view model)

inputStyle : Attribute msg
inputStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]

-- Json.map : (a -> b) -> Decoder a -> Decoder b
-- customDecoder : Decoder a -> (a -> Result String b) -> Decoder b
-- on : String -> Decoder msg -> Attribute msg
-- keyCode : Decoder Int

onEnter : msg -> Attribute msg
onEnter msg =
  on "keydown" (Json.map (always msg) (Json.customDecoder Events.keyCode is13))

is13 : Int -> Result String ()
is13 code =
  if code == 13 then
    Ok ()

  else
    Err "not the right key code"


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch (List.map subWidget model.widgets)

subWidget : GifWidget -> Sub Msg
subWidget {id, model} =
  Sub.map (SubMsg id) (Gif.subscriptions model)

