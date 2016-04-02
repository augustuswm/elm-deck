import Effects exposing (..)
import Html exposing (..)
import Html.Attributes exposing (classList, href)
import Keyboard
import Maybe
import StartApp
import Task

import Component.Deck exposing (Deck)
import ElmDeck exposing (..)

-- Model

type alias State =
  { deck : Deck
  , fullscreen : FullscreenState
  , editing : EditingState
  }

type Action =
  NoOp
  | LoadDeck Deck
  | Forward
  | Backward
  | SetFullscreen FullscreenState
  | SetEditing EditingState

-- Utilities
generalizeDeckAction : Component.Deck.Action -> Action
generalizeDeckAction action =
  case action of
    Component.Deck.Load (Just deck) -> LoadDeck deck
    _ -> NoOp

-- Initialize
init : (State, Effects Action)
init =
  let
    (deck, effect) = Component.Deck.init "My New Deck"
  in
    ( State deck False False
    , Effects.map generalizeDeckAction effect
    )

-- Update

update : Action -> State -> (State, Effects Action)
update action state =
  case action of
    NoOp -> 
      ( state
      , Effects.none
      )

    LoadDeck deck ->
      ( { state | deck = deck }
      , Effects.none
      )

    Forward ->
      let
        (deck, effect) = Component.Deck.update Component.Deck.Forward state.deck
      in
        ( { state | deck = deck }
        , Effects.map generalizeDeckAction effect
        )

    Backward ->
      let
        (deck, effect) = Component.Deck.update Component.Deck.Backward state.deck
      in
        ( { state | deck = deck }
        , Effects.map generalizeDeckAction effect
        )

    SetFullscreen isEnabled ->
      ( { state | fullscreen = isEnabled }
      , Effects.none
      )

    SetEditing isEnabled->
      ( { state | editing = isEnabled}
      , Effects.none
      )

-- View

view : Signal.Address Action -> State -> Html
view address state =
  div [ classList [ ("app-container", True) ] ]
  [ Component.Deck.view state.deck
  , ul [ classList [ ("sub-controls", True) ] ]
    [ li [ classList [ ("sub-control", True) ] ]
      [ a [ href "#", classList [ ("sub-control-edit", True) ] ]
        [ text "Edit "
        , i [ classList [ ("fa fa-pencil", True) ] ] []
        ]
      ]
    , li [ classList [ ("sub-control", True) ] ]
      [ a [ href "#", classList [ ("sub-control-edit", True) ] ]
        [ text "Fullscreen "
        , i [ classList [ ("fa fa-arrows-alt", True) ] ] []
        ]
      ]
    ]
  ]

-- Runner

app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = [ traverse ]
    }

main =
  app.html

-- Signals

traverse : Signal Action
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

-- Tasks

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks