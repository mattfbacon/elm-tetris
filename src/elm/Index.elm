port module Index exposing (main)

import Browser exposing (Document)
import Browser.Events exposing (onKeyDown)
import Components.Board
import Components.Hold
import Components.Next
import Html exposing (text)
import Html.Attributes as Attrs
import Html.Lazy exposing (lazy, lazy2)
import Keybinds as K exposing (Keybind, keyProcessor)
import Keyboard.Event exposing (considerKeyboardEvent)
import Logic exposing (Board, Paused(..), Piece, PieceType, emptyBoard, getLevelFromScore, getSpeedFromLevel)
import Random
import Time
import Util exposing (const)
import String exposing (fromInt)
import Html exposing (Html)


{-| The step of a state machine when it receives its only event—the user pause
keybind.
-}
updatePauseState : Paused -> Paused
updatePauseState oldState =
    case oldState of
        NotPaused ->
            PausedByUser

        PausedByFocus ->
            PausedByUser

        PausedByUser ->
            ReadyToUnpause 3

        ReadyToUnpause _ ->
            PausedByUser


type alias State =
    { board : Board
    , hold : Maybe PieceType
    , hasUsedHold : Bool
    , previousWasTetris : Bool
    , piece : Maybe Piece
    , score : Int
    , bag : List PieceType
    , paused : Paused
    }


main : Program () State Msg
main =
    Browser.document
        { init =
            const
                ( { board = emptyBoard
                  , hold = Nothing
                  , hasUsedHold = False
                  , previousWasTetris = False
                  , piece = Nothing
                  , score = 0
                  , bag = []
                  , paused = NotPaused
                  }
                , Random.generate NewBag Logic.bag
                )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


port windowFocus : (String -> msg) -> Sub msg


subscriptions : State -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model.paused of
            NotPaused ->
                Time.every (model.score |> getLevelFromScore |> getSpeedFromLevel |> ((*) 1000)) Tick

            ReadyToUnpause _ ->
                Time.every 1000 CountUnpause

            PausedByUser ->
                Sub.none

            PausedByFocus ->
                Sub.none
        , onKeyDown <| considerKeyboardEvent (keyProcessor model.paused >> Maybe.map KeyPress)
        , windowFocus VisibilityChange
        ]


type Msg
    = Tick Time.Posix
    | GameTick
    | GetNewPiece
    | NewBag (List PieceType)
    | KeyPress Keybind
    | VisibilityChange String
    | CountUnpause Time.Posix


noCommand : State -> (State, Cmd Msg)
noCommand x = (x, Cmd.none)


updateModelAfterPieceDrops : State -> Board -> (State, Cmd Msg)
updateModelAfterPieceDrops oldModel newBoard =
    let
        ( linesCleared, finalBoard ) =
            Logic.updateBoard newBoard
    in
    { oldModel
        | board = finalBoard
        , piece = Nothing
        , hasUsedHold = False
        , previousWasTetris = linesCleared == 4
        , score =
            oldModel.score
            + Logic.scoreForLinesCleared
                oldModel.previousWasTetris
                linesCleared
    } |> update GetNewPiece -- get the new piece


keyHandler : Keybind -> State -> (State, Cmd Msg)
keyHandler msg model =
    case model.piece of
        Nothing ->
            case msg of
                K.Pause ->
                    { model | paused = updatePauseState model.paused } |> noCommand

                _ ->
                    model |> noCommand

        Just piece ->
            let
                checker =
                    Logic.tryMoves model.board piece
            in
            case msg of
                K.LeftRotate ->
                    { model | piece = Just <| checker <| Logic.alsoTryBumps <| Logic.rotatePieceLeft } |> noCommand

                K.RightRotate ->
                    { model | piece = Just <| checker <| Logic.alsoTryBumps <| Logic.rotatePieceRight } |> noCommand

                K.LeftMove ->
                    { model | piece = Just <| checker <| [ Logic.movePieceLeft ] } |> noCommand

                K.RightMove ->
                    { model | piece = Just <| checker <| [ Logic.movePieceRight ] } |> noCommand

                K.SoftDrop ->
                    { model | piece = Just <| Logic.softDrop model.board piece } |> noCommand

                K.HardDrop ->
                    Logic.hardDrop piece model.board |> updateModelAfterPieceDrops model

                K.Hold ->
                    if model.hasUsedHold then
                        -- no using hold when it's already been used
                        model |> noCommand

                    else
                        { model | piece = Maybe.map Logic.newPiece model.hold, hold = Maybe.map .type_ model.piece, hasUsedHold = True } |> noCommand

                K.Pause ->
                    { model | paused = updatePauseState model.paused } |> noCommand


update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        GameTick ->
            case model.piece of
                Nothing ->
                    update GetNewPiece model

                Just piece ->
                    if piece.locked then
                        Logic.addPieceToBoard piece model.board |> updateModelAfterPieceDrops model

                    else
                        { model | piece = Just (Logic.updatePiece model.board piece) } |> noCommand

        GetNewPiece ->
            case model.bag of
                [] ->
                    ( model, Random.generate NewBag Logic.bag )

                nextPiece :: nextBag ->
                    if List.length nextBag < 6 -- always have at least 6 next pieces
                        then ( model, Random.generate NewBag Logic.bag )
                        else { model | bag = nextBag, piece = Just (Logic.newPiece nextPiece |> Logic.updatePiece model.board) } |> noCommand

        Tick _ ->
            update GameTick model

        NewBag newBag ->
            { model | bag = List.append model.bag newBag } |> update GetNewPiece

        KeyPress keybind ->
           keyHandler keybind model

        VisibilityChange "focus" ->
            case model.paused of
                PausedByFocus ->
                   { model | paused = ReadyToUnpause 3 } |> noCommand

                _ ->
                   model |> noCommand

        VisibilityChange "blur" ->
            case model.paused of
                NotPaused ->
                   { model | paused = PausedByFocus } |> noCommand

                ReadyToUnpause _ ->
                   { model | paused = PausedByFocus } |> noCommand

                _ ->
                   model |> noCommand

        -- invalid message from port
        VisibilityChange _ ->
           model |> noCommand

        CountUnpause _ ->
            ( { model
                | paused =
                    case model.paused of
                        ReadyToUnpause x ->
                            if x <= 1 then
                                NotPaused

                            else
                                ReadyToUnpause (x - 1)

                        _ ->
                            model.paused
              }
            , Cmd.none
            )


scoreHeader : Int -> Html msg
scoreHeader score = Html.h2 [ Attrs.id "title" ] [ text <| "Score: " ++ (fromInt score) ++ "\u{2003}Level: " ++ (fromInt <| Logic.getLevelFromScore score) ]


view : State -> Document Msg
view model =
    { title = "Tetris in Elm"
    , body =
        [ Html.main_ []
            [ lazy scoreHeader model.score
            , Html.h3 [ Attrs.id "hold-title" ] [ text "Hold" ]
            , lazy2 Components.Board.view model.board model.piece
            , lazy Components.Hold.view model.hold
            , lazy Components.Next.view model.bag
            ]
        ]
    }
