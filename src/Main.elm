module Main exposing (..)

import Browser
import Array exposing (..)
import Html exposing (Html, text, div, button, li, ul)
import Html.Attributes exposing (class)
import Html.Events exposing(onClick)
{- import View.Button exposing (btn) -}
import Random
import Random.Array
import Random.List
import List.Extra

---- MODEL ----

defaultCards = [ "1" ,"2" ,"3" ,"4" ,"5" ,"6" ,"7" ,"8", "0"]
rows = [ "2" ,"5" ,"8" ] --used for shenanigans

type alias Model =
    { 
        totalMoves: Int,
        number: Int,
        clickedCardValue: Int,
        cards: List String
    }

initialModel = 
    { 
        totalMoves = 0,
        number = 8,
        cards = defaultCards,
        clickedCardValue = 0
    }

init : ( Model, Cmd Msg )
init =
    ( initialModel, Random.generate MyRandomInt (Random.int 1 8))

cards = initializeCards defaultCards -- randomizeList [2, initialModel.number] defaultCards --Array.fromList (

initializeCards lst =
    lst
        |> randomizeList [2, initialModel.number]


cut : Int -> List a -> List a
cut j list =
   let
       k = modBy (List.length list) j
       (a, b) = List.Extra.splitAt k list
    in
      b ++ a

shuffle : List a -> List a
shuffle list =
    let
      (a, b) = List.Extra.splitAt ((List.length list)//2) list
    in
      List.Extra.interweave a b

splitAndShuffle : Int -> List a -> List a
splitAndShuffle k list =
    list
      |> cut k
      |> shuffle

randomizeList : (List Int) -> List a -> List a
randomizeList integerList list =
    List.foldl splitAndShuffle list integerList

-- check logic --
{- pos 0 -> check 1, 3
pos 1 -> check 0,2,4
pos 2 -> check 1,5
pos 3 -> check 0,4
pos 4 -> check 1,3,5,7
pos 5 -> check 2,4,8
pos 6 -> check 3,7
pos 7 -> check 4,6,8
pos 8 -> check 5,7 -}

---- UPDATE ----


type Msg
    = ResetGame | MyRandomInt Int | GenerateRandomNumberEvent | ShuffleCards | IncrementMoves |
    SendButtonData String| NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of 
        ResetGame -> (initialModel, Random.generate MyRandomInt (Random.int 1 8))

        ShuffleCards -> (model, Cmd.none)

        GenerateRandomNumberEvent -> (model, Random.generate MyRandomInt (Random.int 1 8))

        MyRandomInt theRandomNumber -> ( { model | number = theRandomNumber }, Cmd.none)

        SendButtonData data -> ({model | clickedCardValue = (Maybe.withDefault 0 (String.toInt data))}, Cmd.none)

        IncrementMoves -> ({ model | totalMoves = model.totalMoves + 1 }, Cmd.none)

        NoOp -> ( model, Cmd.none )

---- VIEW ----

btn: String ->  Html Msg
btn txt =
    let
        nr = if txt == "0" then "" else txt
        cls = 
            if txt == "0" then
              "card bg-transparent"
            else
              "card"
    in
      if nr == "" then 
        button [ class cls ] [text nr]
       else
        button [ class cls , onClick (SendButtonData nr)] [text nr]

---- Renders a list of strings ----
{- renderCards : List String -> Html Msg
renderCards lst = 
    div []
    (List.map(\l-> renderRow cards) lst)

renderRow : List String -> Html Msg
renderRow lst =
    div [ class "row"]
    (List.map (\r -> btn r) lst) -}

-- Retrieves an element from the list at position i or "" if not found
getListElement lst i =
    lst
        |> List.Extra.getAt i
        |> Maybe.withDefault ""

-- Receives a index and returns a card --
mapIndexToCard index =
    let
        element = getListElement cards index
    in
        btn element


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [
        div [ class "row" ] [ mapIndexToCard 0, mapIndexToCard 1, mapIndexToCard 2]
        , div [ class "row" ] [ mapIndexToCard 3, mapIndexToCard 4, mapIndexToCard 5]
        , div [ class "row" ] [ mapIndexToCard 6, mapIndexToCard 7, mapIndexToCard 8]
         {- , renderCards rows -}
         {- , div [] [ renderCards cards ] -}
        , div [ class "row space-between" ] 
            [ button [ class "btn" , onClick ResetGame] [text "New Game"]
              , button [ class "btn" , onClick GenerateRandomNumberEvent] [text (String.fromInt model.number)]
              , div [ class "btn" ] [text ("Moves: " ++ (String.fromInt model.totalMoves))]
            ]
        , div [ class "row"] [text ("Clicked: " ++ (String.fromInt model.clickedCardValue))]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
