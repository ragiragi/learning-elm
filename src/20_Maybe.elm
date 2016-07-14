import Html exposing (..)
import Html.Attributes exposing (..)

type alias User =
  { name : String
  , age : Maybe Int
  }


sue : User
sue =
  { name = "Sue", age = Nothing }

tom : User
tom =
  { name = "Tom", age = Just 24 }

alice = User "Alice" (Just 14)

bob = User "Bob" (Just 16)

users = [ sue, tom, alice, bob ]


canBuyAlcohol : User -> Bool
canBuyAlcohol user =
  case user.age of
    Nothing ->
      False

    Just age ->
      age >= 21


getTeenAge : User -> Maybe Int
getTeenAge user =
  case user.age of
    Nothing ->
      Nothing

    Just age ->
      if 13 <= age && age <= 18 then
        Just age
      else
        Nothing



-- TESTS

main =
  div [ style [ ("font-family", "monospace") ] ]
    [ display "canBuyAlcohol tom" (canBuyAlcohol tom)
    , display "canBuyAlcohol sue" (canBuyAlcohol sue)
    , display "List.filterMap getTeenAge users"
        (List.filterMap getTeenAge users)
    ]

display : String -> a -> Html msg
display name value =
  div [] [ text (name ++ " ==> " ++ toString value) ]
