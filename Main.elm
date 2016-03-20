import Array exposing (fromList)
import Graphics.Element exposing (..)
import Html exposing (..)
import Keyboard
import Maybe exposing (Maybe(Just))
import Signal

import Actions exposing (DeckAction (..))
import Deck exposing (create, view)
import Slide exposing (create)
import Types exposing (Deck, Slide)

init : Deck
init =
  Deck.create "My New Deck" "amayo"
    <| fromList [
        Slide.create "The First Title" (Just "#A Slide Header 1")
      , Slide.create "The Second Title" (Just "#A Slide Header 2")
      , Slide.create "The Third Title" (Just "#A Slide Header 3")
      , Slide.create "The Fourth Title" (Just "#A Slide Header 4")
      , Slide.create "The Fifth Title" (Just "#A Slide Header 5")
      ]

model : Signal Deck
model =
  Signal.foldp Deck.update init traverse

traverse : Signal DeckAction
traverse =
  let
    keyToAction key =
      case key of
        100 ->
          Forward
        97 ->
          Backward
        _ ->
          NoOp
  in
    Signal.map keyToAction Keyboard.presses

main : Signal Html
main =
  Signal.map Deck.view model

--main =
--  Signal.map show <| parseSignal traverse

--main =
--  let
--    keyToString key =
--      case key of
--        100 -> "->"
--        97 -> "<-"
--        _ -> ""
--  in
--    Signal.map text <| Signal.map keyToString Keyboard.presses