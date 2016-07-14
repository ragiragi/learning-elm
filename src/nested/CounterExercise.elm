module CounterExercise exposing ( Model, Msg, init, update, view )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)



-- MODEL

maxCount = 5
minCount = -5

type alias Model =
  { count : Int
  , clicks : Int
  }


init : Int -> Model
init count =
  { count = count
  , clicks = 0
  }



-- UPDATE


type Msg
  = Increment
  | Decrement


update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      { model | count = model.count + 1
              , clicks = model.clicks + 1 }

    Decrement ->
      { model | count = model.count - 1
              , clicks = model.clicks + 1 }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ button
        [ onClick Decrement
        , disabled (model.count <= minCount)
        ]
        [ text "-" ]
    , div
      [ countStyle ]
      [ text (toString model.count)
      , span [ clicksStyle ] [ text ("/" ++ (toString model.clicks)) ]
      ]
    , button
        [ onClick Increment
        , disabled (model.count >= maxCount)
        ]
        [ text "+" ]
    ]


countStyle : Attribute msg
countStyle =
  style
    [ ("font-size", "20px")
    , ("font-family", "monospace")
    , ("display", "inline-block")
    , ("width", "50px")
    , ("text-align", "center")
    ]

clicksStyle : Attribute msg
clicksStyle =
  style
    [ ("font-size", "10px")
    , ("color", "blue")
    ]
