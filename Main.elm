module Main where

import Debug exposing (..)
import Effects exposing (..)
import Html exposing (..)
import Html.Attributes exposing (classList, href, title)
import Keyboard
import Maybe
import StartApp
import Task

import Component.Deck exposing (Deck)
import Component.Editor exposing (Editor)
import Component.Slide
import Component.Tools exposing (Tools)
import ElmDeck exposing (..)

-- Model

type alias State =
  { deck : Deck
  , editor : Editor
  , fullscreen : FullscreenState
  }

-- Actions

type Action =
  NoOp
  | LoadDeck Deck
  | SetFullscreen FullscreenState
  | DeckAction Component.Deck.Action
  | SlideAction Component.Slide.Action
  | EditorAction Component.Editor.Action

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
    editor = Editor False False
  in
    ( State deck editor False
    , Effects.map generalizeDeckAction effect
    )

-- Update

update : Action -> State -> (State, Effects Action)
update action state =
  case action of
    NoOp -> ( state, Effects.none )

    LoadDeck deck ->
      ( { state | deck = deck }, Effects.none )

    SetFullscreen isEnabled ->
      ( { state | fullscreen = isEnabled }, Effects.none )

    DeckAction deckAction ->
      if state.editor.focused == False then
        let
          (deck, effect) = Component.Deck.update deckAction state.deck
        in
          ( { state | deck = deck }
          , Effects.map generalizeDeckAction effect
          )
      else
        ( state, Effects.none )

    SlideAction slideAction ->
      let
        (deck, effect) = Component.Deck.update (Component.Deck.SlideAction slideAction) state.deck
      in
        ( { state | deck = deck }
        , Effects.map generalizeDeckAction effect
        )

    EditorAction editorAction ->
      case editorAction of
        Component.Editor.UpdateTitle title ->
          update (SlideAction <| Component.Slide.UpdateTitle title) state

        Component.Editor.UpdateBody title ->
          update (SlideAction <| Component.Slide.UpdateBody title) state

        _ ->
          let
            (editor, editorAction) = Component.Editor.update editorAction state.editor
          in
            ( { state | editor = editor }
            , Effects.none
            ) 

-- View

view : Signal.Address Action -> State -> Html
view address state =
  let
    deck = Component.Deck.view (state.editor.editing, state.deck)
    editor = Component.Editor.view (Signal.forwardTo address EditorAction) (state.editor, state.deck)
    tools = [
      Component.Tools.Add []
    , Component.Tools.Edit []
    , Component.Tools.Fullscreen []
    ]
  in
    div [ classList [ ("app-container", True) ] ]
    [ editor
    , deck
    , div [ classList [ ("sub-controls-wrapper", True) ] ]
      [ (Component.Tools.view tools)]
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
        97 ->
          DeckAction Component.Deck.Backward
        100 ->
          DeckAction Component.Deck.Forward
        101 ->
          EditorAction Component.Editor.ToggleEditing
        115 ->
          DeckAction Component.Deck.AddSlide
        _ ->
          DeckAction Component.Deck.NoOp
  in
    Signal.map keyToAction Keyboard.presses

-- Tasks

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks