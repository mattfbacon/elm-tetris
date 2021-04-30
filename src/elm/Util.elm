module Util exposing
    ( Matrix
    , arrayAll
    , arrayAny
    , arrayConcat
    , arrayIndexedAny
    , const
    , isJust
    , matrixNegate
    , maxXY
    , minXY
    , padFront
    , shuffle
    , vec2DAdd
    , vec2DJoin
    , vec2DMap2
    , vec2DMultiply
    , vec2DNegate
    , vec2DScalarDiv
    )

import Array exposing (Array)
import Html exposing (a)
import Random exposing (Generator)
import String exposing (fromFloat)
import Svg.Attributes exposing (x)


const : b -> a -> b
const val _ =
    val


anyInt : Generator Int
anyInt =
    Random.int Random.minInt Random.maxInt


{-| source: <https://github.com/elm-community/random-extra/blob/master/src/Random/List.elm>
-}
shuffle : List a -> Generator (List a)
shuffle list =
    Random.map
        (\independentSeed ->
            list
                |> List.foldl
                    (\item ( acc, seed ) ->
                        let
                            ( tag, nextSeed ) =
                                Random.step anyInt seed
                        in
                        ( ( item, tag ) :: acc, nextSeed )
                    )
                    ( [], independentSeed )
                |> Tuple.first
                |> List.sortBy Tuple.second
                |> List.map Tuple.first
        )
        Random.independentSeed


type alias Matrix number =
    ( ( number, number ), ( number, number ) )


vec2DMultiply : Matrix number -> ( number, number ) -> ( number, number )
vec2DMultiply ( ( xInX, yInX ), ( xInY, yInY ) ) ( x, y ) =
    ( xInX * x + yInX * y, xInY * x + yInY * y )


vec2DMap2 : (number -> number -> number) -> ( number, number ) -> ( number, number ) -> ( number, number )
vec2DMap2 f ( x1, y1 ) ( x2, y2 ) =
    ( f x1 x2, f y1 y2 )


vec2DAdd : ( number, number ) -> ( number, number ) -> ( number, number )
vec2DAdd =
    vec2DMap2 (+)


matrixNegate : Matrix number -> Matrix number
matrixNegate ( ( a, b ), ( c, d ) ) =
    ( ( -a, -b ), ( -c, -d ) )


vec2DNegate : ( number, number ) -> ( number, number )
vec2DNegate ( x, y ) =
    ( -x, -y )


arrayConcat : Array (Array a) -> Array a
arrayConcat =
    Array.foldr (\arr acc -> Array.append arr acc) Array.empty


arrayAny : (a -> Bool) -> Array a -> Bool
arrayAny pred arr =
    case Array.get 0 arr of
        Nothing ->
            False

        Just x ->
            if pred x then
                True

            else
                arrayAny pred <| Array.slice 1 (Array.length arr) arr


arrayIndexedAny : (Int -> a -> Bool) -> Array a -> Bool
arrayIndexedAny pred =
    Array.indexedMap pred >> arrayAny identity


arrayAll : (a -> Bool) -> Array a -> Bool
arrayAll pred arr =
    case Array.get 0 arr of
        Nothing ->
            True

        Just x ->
            if pred x then
                arrayAll pred <| Array.slice 1 (Array.length arr) arr

            else
                False


padFront : Int -> a -> List a -> List a
padFront newLen padding xs =
    let
        paddingRequired =
            newLen - List.length xs
    in
    case sign paddingRequired of
        Negative ->
            List.drop -paddingRequired xs

        Zero ->
            xs

        Positive ->
            List.append (List.repeat paddingRequired padding) xs


type Sign
    = Negative
    | Zero
    | Positive


sign : Int -> Sign
sign x =
    if x < 0 then
        Negative

    else if x > 0 then
        Positive

    else
        Zero


isJust : Maybe a -> Bool
isJust x =
    case x of
        Nothing ->
            False

        Just _ ->
            True


{-| Since this function uses (0, 0) as its initial value, it's not really
suitable for regular usage, but works just fine for the pieces which all have
their center at (0, 0).
-}
foldXY : ( number, number ) -> (number -> number -> number) -> List ( number, number ) -> ( number, number )
foldXY initial fn xys =
    List.foldr (vec2DMap2 fn) initial xys


minXY : List ( number, number ) -> ( number, number )
minXY =
    foldXY ( 2147483647, 2147483647 ) min


maxXY : List ( number, number ) -> ( number, number )
maxXY =
    foldXY ( -2147483648, -2147483648 ) max


vec2DJoin : String -> ( Float, Float ) -> String
vec2DJoin sep ( x, y ) =
    fromFloat x ++ sep ++ fromFloat y


vec2DScalarDiv : Float -> ( Float, Float ) -> ( Float, Float )
vec2DScalarDiv k ( x, y ) =
    ( x / k, y / k )
