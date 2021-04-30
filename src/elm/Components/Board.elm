module Components.Board exposing (view)

import Array exposing (indexedMap, toList)
import Colors exposing (Color, colorAsHex)
import Components.Common exposing (makeViewBox, minoDef, toSVGPosition, viewPiece)
import Html.Attributes exposing (attribute)
import List exposing (filterMap)
import Logic exposing (boardHeight, boardWidth, visibleRows)
import Svg exposing (Attribute, Svg, defs, g, rect, svg, use)
import Svg.Attributes exposing (fill, height, id, width, x, y)
import Svg.Lazy exposing (lazy)
import Util exposing (arrayConcat)


xlinkHref : String -> Attribute msg
xlinkHref =
    attribute "href"


makeMino : Int -> Int -> Maybe Color -> Maybe (Svg msg)
makeMino row col =
    Maybe.map
        (\type_ ->
            use
                [ col |> toSVGPosition |> x
                , row |> toSVGPosition |> y
                , type_ |> colorAsHex |> fill
                , xlinkHref "#mino"
                ]
                []
        )


viewBoard : Logic.Board -> Svg msg
viewBoard board =
    g [ id "board" ]
        (board
            |> indexedMap (\rowNum -> indexedMap (\col mino -> makeMino rowNum col mino))
            |> arrayConcat
            |> toList
            |> filterMap identity
        )


view : Logic.Board -> Maybe Logic.Piece -> Svg msg
view board piece =
    svg
        [ id "game"
        , makeViewBox 0 (boardHeight - visibleRows) boardWidth visibleRows
        , attribute "xmlns" "http://www.w3.org/2000/svg"
        ]
        [ rect [ x "0", y "0", width <| toSVGPosition <| boardWidth, height <| toSVGPosition <| boardHeight, fill Colors.background ] []
        , defs [] [ minoDef ]
        , lazy viewBoard board
        , lazy viewPiece piece
        ]
