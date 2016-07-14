import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Gif

a = (\n -> n / 2) 128

main =
  App.program
    { init = init "Funny Cats" "Funny Dogs"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model =
  { left : Gif.Model
  , right : Gif.Model
  }

init : String -> String -> ( Model, Cmd Msg )
init leftTopic rightTopic =
  let
    (left, leftEffect) = Gif.init leftTopic
    (right, rightEffect) = Gif.init rightTopic
  in
    ( Model left right
    , Cmd.batch
        [ Cmd.map Left leftEffect
        , Cmd.map Right rightEffect
        ]
    )


-- UPDATE

type Msg
  = Left Gif.Msg
  | Right Gif.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Left gifMsg ->
      let
        (left, leftEffect) = Gif.update gifMsg model.left
      in
        ( Model left model.right
        , Cmd.map Left leftEffect
        )

    Right gifMsg ->
      let
        (right, rightEffect) = Gif.update gifMsg model.right
      in
        ( Model model.left right
        , Cmd.map Right rightEffect
        )


-- VIEW

view : Model -> Html Msg
view model =
  div
    [ style [ ("display", "flex") ]
    ]
    [ App.map Left (Gif.view model.left)
    , App.map Right (Gif.view model.right)
    -- App.map : (Gif.Msg -> Msg) -> (Html Gif.Msg) -> Html Msg
    ]


-- SUBS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Sub.map Left (Gif.subscriptions model.left)
    , Sub.map Right (Gif.subscriptions model.right)
    ]
