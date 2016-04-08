module Main where

import Debug exposing (..)
import Effects exposing (..)
import Html exposing (..)
import Html.Attributes exposing (classList, href, title)
import Html.Events exposing (onClick)
import Keyboard
import Maybe
import Runner
import Signal.Stream exposing (Stream)
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
  , theme : String
  , themeMenu : Bool
  , fullscreen : FullscreenState
  }

-- Actions

type Action =
  NoOp
  | LoadDeck Deck
  | SetTheme String
  | ToggleThemeMenu
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
    ( State deck editor "magula" False False
    , Effects.map generalizeDeckAction effect
    )

-- Update

update : Action -> State -> (State, Effects Action)
update action state =
  case action of
    NoOp -> ( state, Effects.none )

    LoadDeck deck ->
      ( { state | deck = deck }, Effects.none )

    ToggleThemeMenu ->
      ( { state | themeMenu = (not state.themeMenu) }, Effects.none )

    SetTheme theme ->
      ( { state | theme = theme }, Effects.none )

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
        Component.Editor.AddSlide ->
          let
            (deck, effect) = Component.Deck.update Component.Deck.AddSlide state.deck
          in
            ( { state | deck = deck }
            , Effects.map generalizeDeckAction effect
            )

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
    singleton x = [x]

    deck =
      Component.Deck.view (Signal.forwardTo address DeckAction) (state.deck)
    editor =
      Component.Editor.view (Signal.forwardTo address EditorAction) (state.editor, state.deck)
    theme =
      Component.Editor.viewTheme state.theme

    currentThemeMarker theme =
      if (theme == state.theme) then
        i [ classList [ ("fa fa-fw fa-check", True)] ] []
      else
        i [ classList [ ("fa fa-fw", True) ] ] [ text " " ]

    themeToLi theme =
      li [ classList [ ("theme", True) ]
      , onClick address (SetTheme theme)
      ] [ currentThemeMarker theme
      , span [ classList [ ("theme-label", True) ] ] [ text theme ] 
      ]

    showThemeMenu =
      (not state.editor.editing) && state.themeMenu

    themeMenu =
      div [ classList [ ("theme-menu", True), ("hidden", (not showThemeMenu)) ] ]
      [ ul [ classList [ ("theme-list", True) ] ]
        <| List.map themeToLi Component.Editor.themes ]

    tools =
      [ Component.Tools.Theme [ onClick address ToggleThemeMenu ] showThemeMenu

      , Component.Tools.Edit
        [ onClick address (EditorAction Component.Editor.ToggleEditing) ]
        state.editor.editing
      , Component.Tools.Fullscreen [] False
      ]
  in
    div [ classList [ ("app-container", True), ("is-editing", state.editor.editing) ] ]
    [ theme
    , editor
    , deck
    , div [ classList [ ("sub-controls-wrapper", True) ] ] [ (Component.Tools.view tools) ]
    , themeMenu
    ]

-- Runner

app =
  Runner.start
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
          Just (DeckAction Component.Deck.Backward)

        100 ->
          Just (DeckAction Component.Deck.Forward)

        115 ->
          Just (DeckAction Component.Deck.AddSlide)

        101 ->
          Just (EditorAction Component.Editor.ToggleEditing)

        116 ->
          Just (ToggleThemeMenu)

        _ ->
          Nothing
  in
    Signal.filterMap keyToAction NoOp Keyboard.presses

-- Tasks

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks