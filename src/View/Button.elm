module View.Button exposing (btn)

import Html exposing (Html, button, text)
import Html.Attributes exposing (class)

btn: String -> Html msg
btn txt =
    button [ class "card" ] [text txt]