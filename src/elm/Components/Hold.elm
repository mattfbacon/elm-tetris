module Components.Hold exposing (view)

import Colors
import Components.Common exposing (makeViewBoxFloat, minoDef, viewPieceType)
import Html.Attributes exposing (attribute)
import Logic exposing (PieceType)
import Svg exposing (Svg, defs, rect, svg)
import Svg.Attributes exposing (fill, height, id, width, x, y)
import Svg.Lazy exposing (lazy)


view : Maybe PieceType -> Svg msg
view piece =
    svg
        [ id "hold"
        , makeViewBoxFloat -2.5 -2 5 4
        , attribute "xmlns" "http://www.w3.org/2000/svg"
        ]
        [ rect [ x "-25", y "-20", width "50", height "40", fill Colors.background ] []
        , defs [] [ minoDef ]
        , lazy viewPieceType piece
        ]
