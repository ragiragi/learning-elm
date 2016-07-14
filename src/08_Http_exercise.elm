import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Http
import Task
import Json.Decode as Json

main = App.program { init = init
                   , update = update
                   , view = view
                   , subscriptions = subscriptions
                   }

-- MODEL ----------------------------------------------------------------------

type alias Model =
  { topic : String
  , gifUrl : String
  , errorMessage : String
  }


topic = "cats"
predefinedTopics = [ "cats", "dogs", "yummy" ]
waitingImageUrl = "/images/waiting.gif"
errorImageUrl = "/images/error.gif"


init : (Model, Cmd Msg)
init =
  (Model topic waitingImageUrl "", getRandomGif topic)


-- UPDATE ---------------------------------------------------------------------

type Msg
  = MorePlease
  | FetchSucceed String
  | FetchFail Http.Error
  | Topic String
  | ChangeTopic String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      ({ model | gifUrl = waitingImageUrl }, getRandomGif model.topic)

    FetchSucceed newUrl ->
      (Model model.topic newUrl "", Cmd.none)

    FetchFail err ->
      (Model model.topic errorImageUrl (toString err), Cmd.none)

    Topic topic ->
      ({ model | topic = topic }, Cmd.none)

    ChangeTopic topic ->
      (Model topic waitingImageUrl "", getRandomGif topic)


getRandomGif : String -> Cmd Msg
getRandomGif topic =
  let
    url =
      "http://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
  in
    Task.perform FetchFail FetchSucceed (Http.get decodeGifUrl url)


decodeGifUrl : Json.Decoder String
decodeGifUrl =
  Json.at ["data", "image_url"] Json.string


-- VIEW -----------------------------------------------------------------------

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text "Choose a topic" ]
    , topicSelect model
    , h3 [] [ text "Or type manually"]
    , input [ type' "text", onInput Topic, value model.topic ] []
    , button [ onClick MorePlease ] [ text "Confirm" ]
    , viewError model.errorMessage
    , button [ onClick MorePlease ] [ text "More Please!" ]
    , div [] [ img [ src model.gifUrl ] [] ]
    ]

topicSelect : Model -> Html Msg
topicSelect model =
  let
    isPredefinedTopic = List.member model.topic predefinedTopics
  in
    select
      [ onInput ChangeTopic ]
      (
        List.map (\topic -> topicOption topic model) predefinedTopics
        ++
        [ option [ value model.topic, selected (not isPredefinedTopic) ]
                 [ text "custom" ] ]
      )

topicOption : String -> Model -> Html Msg
topicOption topic model =
  option [ value topic, selected (topic == model.topic) ] [ text topic ]

viewError err =
  if err == ""
  then text ""
  else h3 [ style [ ("color", "red") ] ] [ text err ]


-- SUBSCRIPTIONS --------------------------------------------------------------

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

