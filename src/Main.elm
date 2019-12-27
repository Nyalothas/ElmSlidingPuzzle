module Main exposing (..)

import Browser
import Array exposing (..)
import Html exposing (Html, text, div, button, li, ul)
import Html.Attributes exposing (class)
import Html.Events exposing(onClick)
import View.Button exposing (btn)
import Random
import Random.Array
import Random.List
import List.Extra

---- MODEL ----

defaultCards = [ "1" ,"2" ,"3" ,"4" ,"5" ,"6" ,"7" ,"8" ]
rows = [ "2" ,"5" ,"8" ] --used for shenanigans

type alias Model =
    { totalMoves: Int,
      number: Int }

initialModel = 
    { totalMoves = 0,
     number = 8 }

init : ( Model, Cmd Msg )
init =
    ( initialModel, Random.generate MyRandomInt (Random.int 1 8) )

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

dealersMove : Int -> List a -> List a
dealersMove k list =
    list
      |> cut k
      |> shuffle

randomizeList : (List Int) -> List a -> List a
randomizeList integerList list =
    List.foldl dealersMove list integerList

---- UPDATE ----


type Msg
    = ResetGame | MyRandomInt Int | GenerateRandomNumberEvent | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of 
        ResetGame -> (initialModel, Random.generate MyRandomInt (Random.int 1 8))

        GenerateRandomNumberEvent -> (model, Random.generate MyRandomInt (Random.int 1 8))

        MyRandomInt theRandomNumber -> ( { model | number = theRandomNumber }, Cmd.none)

        NoOp -> ( model, Cmd.none )

{- 
    ResetMoves moves -> ( model totalMoves, Cmd.none) -}


---- VIEW ----

{- renderList : List String -> Html msg
renderList lst =
    ul []
        (List.map (\l -> li [] [ text l ]) lst) -}

---- Renders a list of strings ----
renderCards : List String -> Html msg
renderCards lst = 
    div []
    (List.map(\l-> renderRow cards) lst)

renderRow : List String -> Html msg
renderRow lst =
    div [ class "row"]
    (List.map (\r -> btn r) lst)

-- Retrieves an element from the list at position i or NaN if not found
getListElement lst i =
    lst
        |> List.Extra.getAt i
        |> Maybe.withDefault "NaN"

view : Model -> Html Msg
view model =
    div [ class "main" ]
        [
        div [ class "row" ] [ btn (getListElement cards 0), btn (getListElement cards 1), btn (getListElement cards 2)]
        , div [ class "row" ] [ btn (getListElement cards 3), btn (getListElement cards 4), btn (getListElement cards 5)]
        , div [ class "row" ] [ btn (getListElement cards 6), btn (getListElement cards 7)]
         {- , renderCards rows -}
         {- , div [] [ renderCards cards ] -}
        , div [ class "row" ] 
            [ button [ class "btn" , onClick ResetGame] [text "New Game"]
              , button [ class "btn" , onClick GenerateRandomNumberEvent] [text (String.fromInt model.number)]  
            ]
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
