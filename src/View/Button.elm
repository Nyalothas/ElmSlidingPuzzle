module View.Button exposing (btn)

import Html exposing (Html, button, text)
import Html.Attributes exposing (class)
import Html.Events exposing(onClick)

btn: String ->  Html msg
btn txt =
    let
        nr = if txt == "0" then "" else txt
        cls = 
            if txt == "0" then
              "card bg-transparent"
            else
              "card"
    in
      if nr /= "0" then 
        button [ class cls , onClick IncrementMoves] [text nr]
       else
        button [ class cls ] [text nr]