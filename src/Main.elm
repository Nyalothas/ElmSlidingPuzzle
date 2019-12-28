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
defaultStringList = "123456780"

type alias Model =
    { 
        totalMoves: Int,
        number: Int,
        clickedCardValue: Int,
        clickedCardIndex: Int,
        cards: List String,
        zeroIndexValue: Int,
        previousZeroIndexValue: Int,
        won: Bool
    }

initialModel = 
    { 
        totalMoves = 0,
        number = 8,
        cards = defaultCards,
        clickedCardValue = -1,
        clickedCardIndex = -1,
        zeroIndexValue = -1,
        previousZeroIndexValue = -1,
        won = False
    }

init : ( Model, Cmd Msg )
init =
    ( initialModel, Random.generate GenerateRandomNumber (Random.int 1 8))

initializeCards r lst =
    lst
        |> randomizeList [r, initialModel.number]


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

indexValueEqualsZero: Int -> Int -> List Int -> Bool
indexValueEqualsZero index zeroIndexValue cardsList =
    Maybe.withDefault 0 (List.Extra.elemIndex index cardsList) == zeroIndexValue

swapIndexWithZero zeroIndex index cardsList= 
    List.Extra.swapAt zeroIndex index cardsList

-- this has no effect --
swapItself index cardsList =
    List.Extra.swapAt index index cardsList

getZeroIndexValue cardsList = 
    Maybe.withDefault -1 (List.Extra.elemIndex 0 cardsList)

setPreviousZeroIndexValue: Int -> Int
setPreviousZeroIndexValue zeroIndexValue =
    let
        value = 
            if zeroIndexValue /= -1 then zeroIndexValue else -1
    in
        value

checkWinCondition: List String -> Bool
checkWinCondition lst =
    let
        lstString = String.concat lst
        won = 
            if lstString == defaultStringList then True else False
    in
        won

incrementMoves totalMoves won =
    let
        moves = 
            if won == False then totalMoves + 1 else totalMoves
    in
        moves

-- moving cards logic --
slideCard won zeroIndexValue cardIndex cardsList =
    let
        swapped = 
            if won == True then 
                swapItself cardIndex cardsList
            else
                -- pos 0 -> check 1, 3
                if cardIndex == 0 then 
                    if zeroIndexValue == 1 then swapIndexWithZero 1 cardIndex cardsList
                    else if zeroIndexValue == 3 then swapIndexWithZero 3 cardIndex cardsList
                    else swapItself cardIndex cardsList
                -- pos 1 -> check 0,2,4
                else if cardIndex == 1 then 
                    if zeroIndexValue == 0 then swapIndexWithZero 0 cardIndex cardsList
                    else if zeroIndexValue == 2 then swapIndexWithZero 2 cardIndex cardsList
                    else if zeroIndexValue == 4 then swapIndexWithZero 4 cardIndex cardsList
                    else swapItself cardIndex cardsList
                -- pos 2 -> check 1,5
                else if cardIndex == 2 then 
                    if zeroIndexValue == 1 then swapIndexWithZero 1 cardIndex cardsList
                    else if zeroIndexValue == 5 then swapIndexWithZero 5 cardIndex cardsList
                    else swapItself cardIndex cardsList
                -- pos 3 -> check 0,4,6
                else if cardIndex == 3 then 
                    if zeroIndexValue == 0 then swapIndexWithZero 0 cardIndex cardsList
                    else if zeroIndexValue == 4 then swapIndexWithZero 4 cardIndex cardsList
                    else if zeroIndexValue == 6 then swapIndexWithZero 6 cardIndex cardsList
                    else swapItself cardIndex cardsList
                -- pos 4 -> check 1,3,5,7
                else if cardIndex == 4 then 
                    if zeroIndexValue == 1 then swapIndexWithZero 1 cardIndex cardsList
                    else if zeroIndexValue == 3 then swapIndexWithZero 3 cardIndex cardsList
                    else if zeroIndexValue == 5 then swapIndexWithZero 5 cardIndex cardsList
                    else if zeroIndexValue == 7 then swapIndexWithZero 7 cardIndex cardsList
                    else swapItself cardIndex cardsList
                -- pos 5 -> check 2,4,8
                else if cardIndex == 5 then 
                    if zeroIndexValue == 2 then swapIndexWithZero 2 cardIndex cardsList
                    else if zeroIndexValue == 4 then swapIndexWithZero 4 cardIndex cardsList
                    else if zeroIndexValue == 8 then swapIndexWithZero 8 cardIndex cardsList
                    else swapItself cardIndex cardsList
                -- pos 6 -> check 3,7
                else if cardIndex == 6 then 
                    if zeroIndexValue == 3 then swapIndexWithZero 3 cardIndex cardsList
                    else if zeroIndexValue == 7 then swapIndexWithZero 7 cardIndex cardsList
                    else swapItself cardIndex cardsList
                -- pos 7 -> check 4,6,8
                else if cardIndex == 7 then 
                    if zeroIndexValue == 4 then swapIndexWithZero 4 cardIndex cardsList
                    else if zeroIndexValue == 6 then swapIndexWithZero 6 cardIndex cardsList
                    else if zeroIndexValue == 8 then swapIndexWithZero 8 cardIndex cardsList
                    else swapItself cardIndex cardsList
                -- pos 8 -> check 5,7
                else if cardIndex == 8 then 
                    if zeroIndexValue == 5 then swapIndexWithZero 5 cardIndex cardsList
                    else if zeroIndexValue == 7 then swapIndexWithZero 7 cardIndex cardsList
                    else swapItself cardIndex cardsList
                else
                    swapItself cardIndex cardsList
    in
        List.map (\r -> String.fromInt r) swapped

---- UPDATE ----

type Msg
    = ResetGame | GenerateRandomNumber Int | IncrementMoves 
    | SetClickedCardValue String
    | SetClickedCardIndex
    | SlideCards
    | SetZeroIndexValue
    | NoOp

convertListToIntegers lst =
    List.map (\r -> Maybe.withDefault 0 (String.toInt r)) lst

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of 
        ResetGame -> ({
            initialModel | 
            cards = initializeCards (model.zeroIndexValue + 1) defaultCards
            }, Random.generate GenerateRandomNumber (Random.int 1 8))

        GenerateRandomNumber theRandomNumber -> ({
            model | 
            number = theRandomNumber, 
            won = checkWinCondition model.cards
            }, Cmd.none)

        SetClickedCardValue indexData -> update SetClickedCardIndex { model | clickedCardValue = Maybe.withDefault 0 (String.toInt indexData) }

        SetClickedCardIndex -> update SetZeroIndexValue {
            model | clickedCardIndex = Maybe.withDefault 0 (List.Extra.elemIndex model.clickedCardValue (convertListToIntegers model.cards))
            }

        SetZeroIndexValue -> update SlideCards { 
            model | 
            previousZeroIndexValue = setPreviousZeroIndexValue model.zeroIndexValue,
            zeroIndexValue = getZeroIndexValue (convertListToIntegers model.cards)
            }

        SlideCards -> update IncrementMoves { 
            model | 
            cards = slideCard model.won model.zeroIndexValue model.clickedCardIndex (convertListToIntegers model.cards)
            }

        IncrementMoves -> ({ 
            model | 
            won = checkWinCondition model.cards,
            totalMoves = incrementMoves model.totalMoves model.won }, Cmd.none)

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
        button [ class cls , onClick (SetClickedCardValue nr)] [text nr]

-- Retrieves an element from the list at position i or "" if not found
getListElement lst i =
    lst
        |> List.Extra.getAt i
        |> Maybe.withDefault ""

-- Receives a index and returns a card --
mapIndexToCard index cards =
    let
        element = getListElement cards index
    in
        btn element

displayWinMessage won totalMoves =
    let
        moves = totalMoves
    in
        if won then
            div [ class "winner" ] [ text ("You Won!🎆😄")]
        else
            div [ class "" ] [ text ("")]


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [
          div [ class "row" ] [ mapIndexToCard 0 model.cards , mapIndexToCard 1 model.cards , mapIndexToCard 2 model.cards ]
        , div [ class "row" ] [ mapIndexToCard 3 model.cards , mapIndexToCard 4 model.cards , mapIndexToCard 5 model.cards ]
        , div [ class "row" ] [ mapIndexToCard 6 model.cards , mapIndexToCard 7 model.cards , mapIndexToCard 8 model.cards ]
        , div [ class "row space-between" ] 
            [ button [ class "btn h-20" , onClick ResetGame] [text "New Game"]
              , displayWinMessage model.won model.totalMoves
              , div [ class "txt h-20" ] [text ("Moves: " ++ (String.fromInt model.totalMoves))]
            ]
        ]

renderList : List String -> Html msg
renderList lst =
        div []
        (List.map (\l -> text (" >> " ++l ++ " ")) lst)

---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
