module Colors exposing (Color(..), background, colorAsHex)


type Color
    = Cyan
    | Blue
    | Orange
    | Yellow
    | Green
    | Purple
    | Red


colorAsHex : Color -> String
colorAsHex color =
    case color of
        Cyan ->
            "#1abc9c"

        Blue ->
            "#2980b9"

        Orange ->
            "#e67e22"

        Yellow ->
            "#f1c40f"

        Green ->
            "#2ecc71"

        Purple ->
            "#9b59b6"

        Red ->
            "#e74c3c"


background : String
background =
    "#2c3e50"
