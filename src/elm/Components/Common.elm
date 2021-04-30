module Components.Common exposing (makeTranslate, makeViewBox, makeViewBoxFloat, minoDef, minoSize, pieceToSVGVanilla, toSVGPosition, viewPiece, viewPieceType)

import Colors exposing (colorAsHex)
import Logic exposing (pieceSize)
import String exposing (fromFloat, fromInt)
import Svg exposing (Attribute, Svg, g, path, rect, use)
import Svg.Attributes exposing (d, fill, fillOpacity, height, id, transform, viewBox, width, x, xlinkHref, y)
import Util exposing (vec2DAdd, vec2DJoin, vec2DNegate, vec2DScalarDiv)


minoSize : Int
minoSize =
    10


minoDef : Svg msg
minoDef =
    g [ id "mino" ]
        [ rect [ x "0", y "0", width "10", height "10", fill "inherit" ] []
        , path [ d "m 0 0 l 2 2 v 6 l -2 2 z", fill "#fff", fillOpacity "0.53" ] [] -- left
        , path [ d "m 0 0 l 2 2 h 6 l 2 -2 z", fill "#fff", fillOpacity "0.75" ] [] -- top
        , path [ d "m 10 10 l -2 -2 v -6 l 2 -2 z", fill "#000", fillOpacity "0.2" ] [] -- right
        , path [ d "m 10 10 l -2 -2 h -6 l -2 2 z", fill "#000", fillOpacity "0.3" ] [] -- bottom
        ]


viewPiece : Maybe Logic.Piece -> Svg msg
viewPiece piece_ =
    case piece_ of
        Nothing ->
            g [ id "piece" ] []

        Just piece ->
            pieceToSVG piece


viewPieceType : Maybe Logic.PieceType -> Svg msg
viewPieceType piece_ =
    case piece_ of
        Nothing ->
            g [ id "piece" ] []

        Just piece ->
            pieceToSVGVanilla piece


locationToMino : ( Int, Int ) -> Svg msg
locationToMino ( xCoord, yCoord ) =
    use
        [ xCoord |> toSVGPosition |> x
        , yCoord |> toSVGPosition |> y
        , fill "inherit"
        , xlinkHref "#mino"
        ]
        []


pieceToSVG : Logic.Piece -> Svg msg
pieceToSVG piece =
    g
        [ piece.type_ |> Logic.pieceName |> id
        , piece.type_ |> Logic.pieceColor |> colorAsHex |> fill
        , id "piece"
        ]
        (piece |> Logic.fullPieceAsBlocks |> List.map locationToMino)


pieceToSVGVanilla : Logic.PieceType -> Svg msg
pieceToSVGVanilla piece =
    g
        [ piece |> Logic.pieceName |> id
        , piece |> Logic.pieceColor |> colorAsHex |> fill
        , id "piece"
        , pieceSize piece |> Tuple.mapBoth toFloat toFloat |> vec2DScalarDiv -2 |> makeTranslate
        ]
        (piece |> Logic.pieceAsBlocks |> List.map (vec2DAdd (Logic.pieceLeftCorner piece |> vec2DNegate) >> locationToMino))


toSVGPosition : Int -> String
toSVGPosition =
    (*) minoSize >> fromInt


makeViewBox : Int -> Int -> Int -> Int -> Attribute msg
makeViewBox x y w h =
    viewBox <| String.join " " <| List.map toSVGPosition [ x, y, w, h ]


makeViewBoxFloat : Float -> Float -> Float -> Float -> Attribute msg
makeViewBoxFloat x y w h =
    viewBox <| String.join " " <| List.map toSVGPositionFloat [ x, y, w, h ]


toSVGPositionFloat : Float -> String
toSVGPositionFloat =
    (*) (toFloat minoSize) >> fromFloat


makeTranslate : ( Float, Float ) -> Attribute msg
makeTranslate ( x, y ) =
    transform <| "translate(" ++ vec2DJoin "," ( x * toFloat minoSize, y * toFloat minoSize ) ++ ")"
