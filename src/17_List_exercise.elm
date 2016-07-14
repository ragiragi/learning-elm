import Html exposing (..)
import Html.Attributes exposing (..)


{------------------------------------------------------------------------------
Exercise:
take a look at the List library and see if you can implement
some of those functions yourself.
-}

type List a = Empty | Node a (List a)

isEmpty : List a -> Bool
isEmpty list =
  case list of
    Empty ->
      True

    Node n remainings ->
      False


cons : a -> List a -> List a
cons n list =
  Node n list


append : List a -> List a -> List a
append firstOne secondOne =
  case (firstOne, secondOne) of
    (Empty, Empty) ->
      Empty

    (Empty, Node n' rests') ->
      secondOne

    (Node n rests, Empty) ->
      firstOne

    (Node n rests, Node n' rests') ->
      cons n (append rests secondOne)


push : a -> List a -> List a
push n list =
  append list (Node n Empty)


reverse : List a -> List a
reverse list =
  case list of
    Empty ->
      Empty

    Node n rest ->
      push n (reverse rest)


take : Int -> List a -> List a
take cnt list =
  case list of
    Empty ->
      Empty

    Node n rest ->
      if cnt > 0 then
        cons n (take (cnt - 1) rest)
      else
        Empty


foldl : (a -> b -> b) -> b -> List a -> b
foldl reducer acc list =
  case list of
    Empty ->
      acc

    Node n rest ->
      foldl reducer (reducer n acc) rest


sum : List number -> number
sum list =
  foldl (\a -> \b -> a + b) 0 list


-- TESTS

main =
  let
    oneTwo = (Node 1 (Node 2 Empty))
    threeFour = (Node 3 (Node 4 Empty))
    oneTwoThree = (Node 1 (Node 2 (Node 3 Empty)))
  in
    div [ style [ ("font-family", "monospace") ] ]
      [ display "isEmpty []" (isEmpty Empty)
      , display "isEmpty [1,2]" (isEmpty oneTwo)
      , display "cons 2 [3,4]" (cons 2 threeFour)
      , display "append [1,2] [3,4]" (append oneTwo threeFour)
      , display "push 3 [1,2]" (push 3 oneTwo)
      , display "reverse [1,2,3]" (reverse oneTwoThree)
      , display "take 2 [1,2,3]" (take 2 oneTwoThree)
      , display "sum(using foldl) [1,2,3]" (sum oneTwoThree)
      ]

display : String -> a -> Html msg
display name value =
  div [] [ text (name ++ " ==> " ++ toString value) ]
