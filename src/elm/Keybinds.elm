module Keybinds exposing (Keybind(..), keyProcessor)

import Keyboard.Event exposing (KeyboardEvent)
import Logic exposing (Paused(..))
import Util exposing (const)


{-| Translation from a key identifier string to a keybind.
-}
normalKeyProcessor : String -> Maybe Keybind
normalKeyProcessor key =
    case key of
        "ArrowLeft" ->
            Just LeftMove

        "ArrowRight" ->
            Just RightMove

        "ArrowUp" ->
            Just RightRotate

        "x" ->
            Just RightRotate

        "ArrowDown" ->
            Just SoftDrop

        " " ->
            Just HardDrop

        "Shift" ->
            Just Hold

        "c" ->
            Just Hold

        "Control" ->
            Just LeftRotate

        "z" ->
            Just LeftRotate

        "Escape" ->
            Just Pause

        "F1" ->
            Just Pause

        -- numpad controls
        "0" ->
            Just Hold

        "1" ->
            Just RightRotate

        "2" ->
            Just SoftDrop

        "3" ->
            Just LeftRotate

        "4" ->
            Just LeftMove

        "5" ->
            Just RightRotate

        "6" ->
            Just RightMove

        "7" ->
            Just LeftRotate

        "8" ->
            Just HardDrop

        "9" ->
            Just RightRotate

        _ ->
            Nothing


{-| A key processor that filters out game-related events from the normal
key processor.
-}
pausedKeyProcessor : String -> Maybe Keybind
pausedKeyProcessor =
    normalKeyProcessor
        >> Maybe.andThen
            (\keybind ->
                case keybind of
                    Pause ->
                        Just Pause

                    _ ->
                        Nothing
            )


{-| Uses the pause state to delegate processing to either `pausedKeyProcessor`
or `normalKeyProcessor`.
-}
keyProcessor : Paused -> KeyboardEvent -> Maybe Keybind
keyProcessor pauseState =
    .key
        >> Maybe.andThen
            (case pauseState of
                NotPaused ->
                    normalKeyProcessor

                ReadyToUnpause _ ->
                    pausedKeyProcessor

                PausedByUser ->
                    pausedKeyProcessor

                PausedByFocus ->
                    const Nothing
            )


type Keybind
    = LeftRotate
    | RightRotate
    | RightMove
    | LeftMove
    | SoftDrop
    | HardDrop
    | Hold
    | Pause
