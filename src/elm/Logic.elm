module Logic exposing
    ( Board
    , Direction(..)
    , OriginType(..)
    , Paused(..)
    , Piece
    , PieceType(..)
    , Position
    , addPieceToBoard
    , allBumps
    , allPieces
    , alsoTryBumps
    , bag
    , boardHeight
    , boardWidth
    , collisionTest
    , directionToAngle
    , emptyBoard
    , fullPieceAsBlocks
    , getLevelFromScore
    , getSpeedFromLevel
    , getX
    , getY
    , hardDrop
    , mapPosition
    , mapX
    , mapY
    , movePieceDown
    , movePieceLeft
    , movePieceRight
    , movePieceUp
    , newPiece
    , normalizePiece
    , originCoords
    , pieceAsBlocks
    , pieceColor
    , pieceIsDown
    , pieceLeftCorner
    , pieceName
    , pieceOrigin
    , pieceSize
    , rotateDirectionLeft
    , rotateDirectionRight
    , rotatePieceLeft
    , rotatePieceRight
    , scoreForLinesCleared
    , softDrop
    , tryMove
    , tryMoves
    , updateBoard
    , updatePiece
    , visibleRows
    )

import Array exposing (Array, indexedMap)
import Colors exposing (Color(..))
import Random exposing (Generator)
import Set
import Util exposing (Matrix, arrayAll, arrayIndexedAny, isJust, matrixNegate, minXY, padFront, shuffle, vec2DAdd, vec2DMultiply)


type Paused
    = NotPaused
    | PausedByFocus
    | PausedByUser
    | ReadyToUnpause Int -- counts the seconds


type Direction
    = Up
    | Down
    | Left
    | Right


directionToAngle : Direction -> Int
directionToAngle direction =
    case direction of
        Up ->
            0

        Right ->
            90

        Down ->
            180

        Left ->
            -90


directionToRotationMatrix : Direction -> Matrix Int
directionToRotationMatrix direction =
    case direction of
        Up ->
            ( ( 1, 0 ), ( 0, 1 ) )

        Right ->
            ( ( 0, -1 ), ( 1, 0 ) )

        Down ->
            matrixNegate <| directionToRotationMatrix Up

        Left ->
            matrixNegate <| directionToRotationMatrix Right


rotateDirectionLeft : Direction -> Direction
rotateDirectionLeft direction =
    case direction of
        Up ->
            Left

        Right ->
            Up

        Down ->
            Right

        Left ->
            Down


rotateDirectionRight : Direction -> Direction
rotateDirectionRight direction =
    case direction of
        Up ->
            Right

        Right ->
            Down

        Down ->
            Left

        Left ->
            Up


{-| Since the coordinates refer to the top-left corner of each mino, when
the minos are rotated the corners also move. For example:

╔─┐ (-- rotate right ->) ┌─╗
└─┘ (-- rotate right ->) └─┘

Thus this needs to be corrected for if we always use the coordinate as the
top-left corner. For the right rotation, we shift it one to the left, as can
be seen in the example as being the top-left corner of the resulting shape.

The directional correction depends on the origin type due to the differing ways
that rotation is implemented for them.

-}
directionalCorrection : OriginType -> Direction -> ( Int, Int )
directionalCorrection origin direction =
    case origin of
        GridOrigin ->
            case direction of
                Up ->
                    ( 0, 0 )

                Right ->
                    ( -1, 0 )

                Down ->
                    ( -1, -1 )

                Left ->
                    ( 0, -1 )

        OffsetOrigin ->
            case direction of
                Up ->
                    ( 0, 0 )

                Right ->
                    ( 0, 1 )

                Down ->
                    ( -1, 1 )

                Left ->
                    ( -1, 0 )


rotateCoords : Direction -> OriginType -> List Position -> List Position
rotateCoords direction origin =
    case origin of
        GridOrigin ->
            List.map <|
                (vec2DMultiply <| directionToRotationMatrix direction)
                    >> (vec2DAdd <| directionalCorrection origin direction)

        OffsetOrigin ->
            List.map <|
                (Tuple.mapBoth
                    ((*) 2 >> (+) 1)
                    ((*) 2 >> (+) 1)
                    >> (vec2DMultiply <| directionToRotationMatrix direction)
                    >> Tuple.mapBoth ((\x -> x // 2) << (+) -1) ((\x -> x // 2) << (+) -1)
                )
                    >> (vec2DAdd <| directionalCorrection origin direction)


type PieceType
    = IBlock
    | JBlock
    | LBlock
    | OBlock
    | SBlock
    | TBlock
    | ZBlock


pieceName : PieceType -> String
pieceName piece =
    case piece of
        IBlock ->
            "IBlock"

        JBlock ->
            "JBlock"

        LBlock ->
            "LBlock"

        OBlock ->
            "OBlock"

        SBlock ->
            "SBlock"

        TBlock ->
            "TBlock"

        ZBlock ->
            "ZBlock"


pieceColor : PieceType -> Color
pieceColor piece =
    case piece of
        IBlock ->
            Cyan

        JBlock ->
            Blue

        LBlock ->
            Orange

        OBlock ->
            Yellow

        SBlock ->
            Green

        TBlock ->
            Purple

        ZBlock ->
            Red


allPieces : List PieceType
allPieces =
    [ IBlock
    , JBlock
    , LBlock
    , OBlock
    , SBlock
    , TBlock
    , ZBlock
    ]


type alias Position =
    ( Int, Int )


getX : Position -> Int
getX =
    Tuple.first


getY : Position -> Int
getY =
    Tuple.second


mapX : (Int -> Int) -> Position -> Position
mapX =
    Tuple.mapFirst


mapY : (Int -> Int) -> Position -> Position
mapY =
    Tuple.mapSecond


type alias Piece =
    { type_ : PieceType
    , orientation : Direction
    , position : Position
    , locked : Bool
    }


newPiece : PieceType -> Piece
newPiece type_ =
    Piece type_ Up ( 5, 20 ) False


mapPosition : (Position -> Position) -> Piece -> Piece
mapPosition fn piece =
    { piece | position = fn piece.position }


minosByCol : List Position -> Array (List Int)
minosByCol blocks =
    Array.initialize boardWidth
        (\col ->
            List.filterMap
                (\mino ->
                    if getX mino == col then
                        Just <| getY mino

                    else
                        Nothing
                )
                blocks
        )


rotatePieceLeft : Piece -> Piece
rotatePieceLeft piece =
    { piece | orientation = piece.orientation |> rotateDirectionLeft }


rotatePieceRight : Piece -> Piece
rotatePieceRight piece =
    { piece | orientation = piece.orientation |> rotateDirectionRight }


movePieceUp : Piece -> Piece
movePieceUp =
    mapPosition <| mapY ((+) -1)


movePieceLeft : Piece -> Piece
movePieceLeft =
    mapPosition <| mapX ((+) -1)


movePieceRight : Piece -> Piece
movePieceRight =
    mapPosition <| mapX ((+) 1)


movePieceDown : Piece -> Piece
movePieceDown =
    mapPosition <| mapY ((+) 1)


allBumps : List (Piece -> Piece)
allBumps =
    [ movePieceUp, movePieceLeft, movePieceRight, movePieceDown ]


alsoTryBumps : (Piece -> Piece) -> List (Piece -> Piece)
alsoTryBumps fn =
    fn :: List.map ((>>) fn) allBumps


tryEach : (b -> Bool) -> List (a -> b) -> a -> Maybe b
tryEach checker options data =
    case options of
        [] ->
            Nothing

        fn :: fns ->
            let
                result =
                    fn data
            in
            if checker result then
                Just result

            else
                tryEach checker fns data


tryMove : Board -> Piece -> (Piece -> Piece) -> Maybe Piece
tryMove board piece fn =
    let
        tentativePiece =
            fn piece
    in
    if collisionTest board piece then
        Nothing

    else
        Just { tentativePiece | locked = False }


tryMoves : Board -> Piece -> List (Piece -> Piece) -> Piece
tryMoves board piece fns =
    tryEach (collisionTest board >> not) fns piece
        |> Maybe.map (\x -> { x | locked = False })
        -- └ unlock the piece if any of the moves succeeded
        |> Maybe.withDefault piece


lowestMinoByCol : List Position -> Array (Maybe Int)
lowestMinoByCol =
    minosByCol >> Array.map List.maximum


{-| If the `Piece` collides with (overlaps with) any pieces on the `Board`, or
the bounds of the play space.

For a function that checks for resting upon a piece using a similar method,
see `pieceIsDown`.

-}
collisionTest : Board -> Piece -> Bool
collisionTest board piece =
    let
        minos =
            fullPieceAsBlocks piece

        collisionSurface =
            lowestMinoByCol <| minos

        transposedBoard =
            transposeBoard board
    in
    arrayIndexedAny
        (\col ->
            Maybe.map
                (\minoRow ->
                    minoRow
                        >= boardHeight
                        || (arrayIndexedAny
                                (\row boardMino ->
                                    case boardMino of
                                        Nothing ->
                                            False

                                        Just _ ->
                                            minoRow + 1 > row
                                )
                            <|
                                (Array.get col transposedBoard |> Maybe.withDefault Array.empty)
                           )
                )
                >> Maybe.withDefault False
        )
        collisionSurface
        || List.any (\( x, _ ) -> x < 0 || x >= boardWidth) minos


{-| If the `Piece` is just above (not overlapping with) any piece on the
`Board`, or the bottom of the play space.

For a function that checks for collisions using a similar method, see
`collisionTest`.

-}
pieceIsDown : Board -> Piece -> Bool
pieceIsDown board piece =
    let
        collisionSurface =
            lowestMinoByCol <| fullPieceAsBlocks piece

        transposedBoard =
            transposeBoard board
    in
    arrayIndexedAny
        (\col ->
            Maybe.map
                (\minoRow ->
                    minoRow
                        == 39
                        || (arrayIndexedAny
                                (\row boardMino ->
                                    case boardMino of
                                        Nothing ->
                                            False

                                        Just _ ->
                                            minoRow + 1 == row
                                )
                            <|
                                (Array.get col transposedBoard |> Maybe.withDefault Array.empty)
                           )
                )
                >> Maybe.withDefault False
        )
        collisionSurface


updatePiece : Board -> Piece -> Piece
updatePiece board piece =
    if pieceIsDown board piece then
        { piece | locked = True }

    else
        { piece | locked = False, position = mapY ((+) 1) piece.position }


softDrop : Board -> Piece -> Piece
softDrop board piece =
    if pieceIsDown board piece then
        piece

    else
        softDrop board <| mapPosition (mapY ((+) 1)) piece


hardDrop : Piece -> Board -> Board
hardDrop piece board =
    let
        droppedPiece =
            softDrop board piece
    in
    addPieceToBoard droppedPiece board


type alias Tile =
    Maybe Color


type alias Row =
    Array Tile


boardWidth : Int
boardWidth =
    10


emptyRow : Row
emptyRow =
    Array.repeat boardWidth Nothing


type alias Board =
    Array Row


boardHeight : Int
boardHeight =
    40


visibleRows : Int
visibleRows =
    20


emptyBoard : Board
emptyBoard =
    Array.repeat boardHeight emptyRow


mapBoard : (Int -> Int -> Tile -> Tile) -> Board -> Board
mapBoard fn =
    indexedMap (\y -> indexedMap (\x -> fn x y))


addPieceToBoard : Piece -> Board -> Board
addPieceToBoard piece =
    let
        pieceBlocks =
            Set.fromList <| fullPieceAsBlocks piece

        insertingColor =
            pieceColor piece.type_
    in
    mapBoard
        (\x y mino ->
            if Set.member ( x, y ) pieceBlocks then
                Just insertingColor

            else
                mino
        )


rowIsFull : Row -> Bool
rowIsFull =
    arrayAll isJust >> not


{-| Returns (the number of rows cleared, the new board)
-}
updateBoard : Board -> ( Int, Board )
updateBoard board =
    let
        clearedBoard =
            Array.toList board |> List.filter rowIsFull
    in
    ( 40 - List.length clearedBoard, clearedBoard |> padFront boardHeight emptyRow |> Array.fromList )


transposeBoard : Board -> Array (Array Tile)
transposeBoard board =
    Array.foldl
        (\row acc ->
            Array.indexedMap
                (\col item ->
                    Array.append (Maybe.withDefault Array.empty <| Array.get col acc) (Array.fromList [ item ])
                 -- the default case will never happen ┘
                )
                row
        )
        (Array.repeat boardWidth Array.empty)
        board


bag : Generator (List PieceType)
bag =
    shuffle allPieces


pieceAsBlocks : PieceType -> List Position
pieceAsBlocks piece =
    case piece of
        IBlock ->
            [ ( -2, -1 ), ( -1, -1 ), ( 0, -1 ), ( 1, -1 ) ]

        JBlock ->
            [ ( -2, -1 ), ( -2, 0 ), ( -1, 0 ), ( 0, 0 ) ]

        LBlock ->
            [ ( -2, 0 ), ( -1, 0 ), ( 0, 0 ), ( 0, -1 ) ]

        OBlock ->
            [ ( -1, -1 ), ( -1, 0 ), ( 0, -1 ), ( 0, 0 ) ]

        SBlock ->
            [ ( -2, -1 ), ( -1, -1 ), ( -1, 0 ), ( 0, 0 ) ]

        TBlock ->
            [ ( -2, 0 ), ( -1, 0 ), ( 0, 0 ), ( -1, -1 ) ]

        ZBlock ->
            [ ( -2, 0 ), ( -1, 0 ), ( -1, -1 ), ( 0, -1 ) ]


pieceLeftCorner : PieceType -> Position
pieceLeftCorner =
    pieceAsBlocks >> minXY


pieceSize : PieceType -> ( Int, Int )
pieceSize piece =
    let
        pieceBlocks =
            pieceAsBlocks piece
    in
    Util.vec2DMap2 (-) (Util.maxXY pieceBlocks) (Util.minXY pieceBlocks) |> vec2DAdd ( 1, 1 )


fullPieceAsBlocks : Piece -> List Position
fullPieceAsBlocks piece =
    pieceAsBlocks piece.type_
        |> rotateCoords piece.orientation (pieceOrigin piece.type_)
        |> List.map
            (Tuple.mapBoth
                ((+) (getX piece.position))
                ((+) (getY piece.position))
            )


type OriginType
    = GridOrigin
    | OffsetOrigin


pieceOrigin : PieceType -> OriginType
pieceOrigin piece =
    case piece of
        IBlock ->
            GridOrigin

        JBlock ->
            OffsetOrigin

        LBlock ->
            OffsetOrigin

        OBlock ->
            GridOrigin

        SBlock ->
            OffsetOrigin

        TBlock ->
            OffsetOrigin

        ZBlock ->
            OffsetOrigin


originCoords : OriginType -> ( Float, Float )
originCoords origin =
    case origin of
        GridOrigin ->
            ( 0, 0 )

        OffsetOrigin ->
            ( -0.5, -0.5 )


normalizePiece : Piece -> Piece
normalizePiece piece =
    { piece | orientation = Up, position = ( 0, 0 ) }


{-| For any given level L you need to earn 5\*L points to move on to the next
level. Therefore the number of points you will have total at any given level
is an arithmetic sequence where a\_1 = 0, a\_n = 5n, and n = level + 1. To get
the level from the points requires finding the inverse of that series which
has been done below (pointfree of course).
-}
getLevelFromScore : Int -> Int
getLevelFromScore =
    toFloat >> (*) (8 / 5) >> (+) 1 >> sqrt >> (+) -1 >> (*) 0.5 >> floor


{-| Taken from Tetris Worlds.
-}
getSpeedFromLevel : Int -> Float
getSpeedFromLevel level =
    let
        floatLevel =
            toFloat level
    in
    (0.8 - (floatLevel * 0.007)) ^ floatLevel


scoreForLinesCleared : Bool -> Int -> Int
scoreForLinesCleared previousWasTetris linesCleared =
    case linesCleared of
        0 ->
            0

        -- base case
        1 ->
            1

        2 ->
            3

        3 ->
            5

        4 ->
            if previousWasTetris then
                12

            else
                8

        _ ->
            0



-- not possible
