module Components.Next exposing (view)

import Components.Hold
import Html exposing (Html, div, h3, section, text)
import Html.Attributes exposing (class, id)
import Logic


view : List Logic.PieceType -> Html msg
view bag =
    section [ id "next" ]
        [ h3 [] [ text "Next" ]
        , div [ class "pieces" ] (List.map (Just >> Components.Hold.view) bag)
        ]
