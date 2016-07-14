import Html exposing (..)
import Html.Attributes exposing (style)

-- TREES

type Tree a
  = Empty
  | Node a (Tree a) (Tree a)

empty : Tree a
empty =
  Empty

singleton : a -> Tree a
singleton v =
  Node v Empty Empty

insert : comparable -> Tree comparable -> Tree comparable
insert x tree =
  case tree of
    Empty ->
      singleton x

    Node y left right ->
      if x > y then
        Node y left (insert x right)

      else if x < y then
        Node y (insert x left) right

      else
        tree

fromList : List comparable -> Tree comparable
fromList xs =
  List.foldl insert empty xs

depth : Tree a -> Int
depth tree =
  case tree of
    Empty -> 0

    Node v left right ->
      1 + (max (depth left) (depth right))

map : (a -> b) -> Tree a -> Tree b
map f tree =
  case tree of
    Empty -> Empty

    Node v left right ->
      Node (f v) (map f left) (map f right)

deepTree =
  fromList [1, 2, 3]

niceTree =
  fromList [2, 1, 3]


-- EXERCISES

-- (1) Sum all of the elements of a tree.
sum' : Tree number -> number
sum' tree =
  case tree of
    Empty -> 0
    Node v left right ->
      v + (sum' left) + (sum' right)


-- (2) Flatten a tree into a list.
flatten' : Tree a -> List a
flatten' tree =
  case tree of
    Empty -> []

    Node v left right ->
      List.concat [flatten' left, [v], flatten' right]

-- (3) Check to see if an element is in a given tree.
isElement' : a -> Tree a -> Bool
isElement' x tree =
  case tree of
    Empty -> False

    Node v left right ->
      (x == v) || (isElement' x left) || (isElement' x right)


{- (4) Write a general fold function that acts on trees. The fold
       function does not need to guarantee a particular order of
       traversal.
-}
fold : (a -> b -> b) -> b -> Tree a -> b
fold reducer initVal tree =
  case tree of
    Empty -> initVal

    Node n left right ->
      let
        rFolded = fold reducer initVal right
        nFolded = reducer n rFolded
      in
        fold reducer nFolded left


{- (5) Use "fold" to do exercises 1-3 in one line each. The best
       readable versions I have come up have the following length
       in characters including spaces and function name:
         sum: 16
         flatten: 21
         isElement: 46
       See if you can match or beat me! Don't forget about currying
       and partial application!
-}
sum : Tree number -> number
sum = fold (+) 0


flatten : Tree a -> List a
flatten = fold (::) []


isElement : a -> Tree a -> Bool
isElement v = fold ((==) v >> (||)) False


-- (6) Can "fold" be used to implement "map" or "depth"?

-- fold does not guarantee of traversal order and
-- it does not provide any tree's structure infomation.
-- so fold cannot be used to implement "map" and "depth"


-- (7) Try experimenting with different ways to traverse a
--       tree: pre-order, in-order, post-order, depth-first, etc.
--       More info at: http://en.wikipedia.org/wiki/Tree_traversal


foldPreorder : (a -> b -> b) -> b -> Tree a -> b
foldPreorder reducer initVal tree =
  case tree of
    Empty -> initVal

    Node n left right ->
      let
        fold = foldPreorder reducer
      in
        ((reducer n initVal |> fold) left |> fold) right


foldInorder : (a -> b -> b) -> b -> Tree a -> b
foldInorder reducer initVal tree =
  case tree of
    Empty -> initVal

    Node n left right ->
      let
        fold = foldInorder reducer
      in
        (fold initVal left |> reducer n |> fold) right

foldPostorder : (a -> b -> b) -> b -> Tree a -> b
foldPostorder reducer initVal tree =
  case tree of
    Empty -> initVal

    Node n left right ->
      let
        fold = foldPostorder reducer
      in
        ((fold initVal left |> fold ) right) |> reducer n




push : a -> List a -> List a
push item list =
  list ++ [ item ]

{-
       F
    /     \
   B       G
  / \       \
 A   D       I
    / \     /
   C   E   H
-}
sampleTree = Node 'F'
              (Node 'B'
                (Node 'A' Empty Empty)
                (Node 'D'
                  (Node 'C' Empty Empty)
                  (Node 'E' Empty Empty)))
              (Node 'G'
                Empty
                (Node 'I'
                  (Node 'H' Empty Empty)
                  Empty))


-- TEST

main =
  div [ style [ ("font-family", "monospace") ] ]
    [ display "depth deepTree" (depth deepTree)
    , display "depth niceTree" (depth niceTree)
    , display "incremented deepTree" (map (\n -> n + 1) deepTree)
    , display "(1) sum deepTree" (sum' deepTree)
    , display "(2) flatten niceTree" (flatten' niceTree)
    , display "(3-1) isElement 2 deepTree" (isElement' 2 deepTree)
    , display "(3-2) isElement 5 deepTree" (isElement' 5 deepTree)
    , display "(5-1) sum (fold) deepTree" (sum deepTree)
    , display "(5-2) flatten (fold) niceTree" (flatten niceTree)
    , display "(5-3) isElement (fold) 2 deepTree" (isElement 2 deepTree)
    , display "(7-1) preorder traverse" (foldPreorder push [] sampleTree)
    , display "(7-2) inorder traverse" (foldInorder push [] sampleTree)
    , display "(7-3) postorder traverse" (foldPostorder push [] sampleTree)

    ]

display : String -> a -> Html msg
display name value =
  div [] [ text (name ++ " ==> " ++ toString value) ]
