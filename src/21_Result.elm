import Html exposing (Html, Attribute, div, h2, input, text)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String

main =
  Html.beginnerProgram { model = model, view = view, update = update }

-- MODEL

type alias Model =
  { content : String
  }

model : Model
model =
  { content = "" }



-- UPDATE

type Msg =
  Change String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | content = newContent }


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ onInput Change, placeholder "Enter Your Age" ] []
    , viewValidateAge model.content
    ]

viewValidateAge : String -> Html Msg
viewValidateAge userInputAge =
  if userInputAge == "" then
    text ""
  else
    case String.toInt userInputAge of
      Err msg ->
        h2 [errorStyle] [text msg]

      Ok age ->
        if age < 0 then
          h2 [errorStyle] [text "I bet you are older than that!"]
        else if age > 140 then
          h2 [errorStyle] [text "Seems unlikely..."]
        else
          h2 [] [text "OK!"]

errorStyle = style [ ("color", "red") ]
