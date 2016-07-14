import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random


main =
  Html.program { init = init
               , view = view
               , update = update
               , subscriptions = subscriptions
               }


-- INIT -------------------------------------------
init : (Model, Cmd Msg)
init =
  (Model 1 1, generatePairOfDieFaces)


-- MODEL ------------------------------------------
type alias Model =
  { dieFace1 : Int
  , dieFace2 : Int
  }


-- UPDATE -----------------------------------------
type Msg
  = Roll
  | NewFace (Int, Int)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      (model, generatePairOfDieFaces)

    NewFace (dieFace1, dieFace2) ->
      (Model dieFace1 dieFace2, Cmd.none)

dieFaceGen = Random.int 1 6
generatePairOfDieFaces = Random.generate NewFace (Random.pair dieFaceGen dieFaceGen)


-- SUBSCRIPTIONS ----------------------------------
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW -------------------------------------------
view : Model -> Html Msg
view model =
  div []
    [ dieFace model.dieFace1
    , dieFace model.dieFace2
    , button [ onClick Roll ] [ text "Roll" ]
    ]

dieFace : Int -> Html Msg
dieFace num =
  let
    imageUrl = "/images/dieface-" ++ (toString num) ++ ".png"
  in
    img [ src imageUrl, width 100, height 100 ] []
