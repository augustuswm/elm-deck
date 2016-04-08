module Component.Editor where

import Array exposing (get)
import List exposing (head)
import Maybe
import Html exposing (..)
import Html.Attributes exposing (classList, href, key, property, rel, value)
import Html.Events exposing (on, onClick, onFocus, onBlur, targetValue)
import Json.Encode exposing (string)
import Maybe

import Component.Deck exposing (Deck)
import Component.Slide exposing (Slide)
import Component.Tools exposing (Tools)
import ElmDeck exposing (..)

-- Model

type alias Editor =
  { editing : EditingState
  , focused : FocusState
  }

themes : List String
themes =
  [ "agate"
  , "androidstudio"
  , "arduino-light"
  , "arta"
  , "ascetic"
  , "atelier-cave-dark"
  , "atelier-cave-light"
  , "atelier-dune-dark"
  , "atelier-dune-light"
  , "atelier-estuary-dark"
  , "atelier-estuary-light"
  , "atelier-forest-dark"
  , "atelier-forest-light"
  , "atelier-heath-dark"
  , "atelier-heath-light"
  , "atelier-lakeside-dark"
  , "atelier-lakeside-light"
  , "atelier-plateau-dark"
  , "atelier-plateau-light"
  , "atelier-savanna-dark"
  , "atelier-savanna-light"
  , "atelier-seaside-dark"
  , "atelier-seaside-light"
  , "atelier-sulphurpool-dark"
  , "atelier-sulphurpool-light"
  , "brown-paper"
  , "codepen-embed"
  , "color-brewer"
  , "dark"
  , "darkula"
  , "default"
  , "docco"
  , "dracula"
  , "far"
  , "foundation"
  , "github-gist"
  , "github"
  , "googlecode"
  , "grayscale"
  , "gruvbox-dark"
  , "gruvbox-light"
  , "hopscotch"
  , "hybrid"
  , "idea"
  , "ir-black"
  , "kimbie.dark"
  , "kimbie.light"
  , "magula"
  , "mono-blue"
  , "monokai-sublime"
  , "monokai"
  , "obsidian"
  , "paraiso-dark"
  , "paraiso-light"
  , "pojoaque"
  , "qtcreator_dark"
  , "qtcreator_light"
  , "railscasts"
  , "rainbow"
  , "school-book"
  , "solarized-dark"
  , "solarized-light"
  , "sunburst"
  , "tomorrow-night-blue"
  , "tomorrow-night-bright"
  , "tomorrow-night-eighties"
  , "tomorrow-night"
  , "tomorrow"
  , "vs"
  , "xcode"
  , "zenburn"
  ]

-- Actions

type Action =
  SetFocus FocusState
  | ToggleEditing
  | UpdateTitle String
  | UpdateBody String
  | AddSlide

-- Update

update : Action -> Editor -> (Editor, Action)
update action editor = 
  case action of
    SetFocus isFocused ->
      ( { editor | focused = isFocused }, action )

    ToggleEditing ->
      let
        newEditor = if editor.focused then editor else { editor | editing = (not editor.editing) }
      in
        ( newEditor, action )

    _ -> ( editor, action )

-- View

viewTheme : String -> Html
viewTheme theme =
  node "link"
  [ rel "stylesheet"
  , href ("/assets/css/code/" ++ theme ++ ".css")
  ] []

view : Signal.Address Action -> (Editor, Deck) -> Html
view address (editor, deck) =
  let
    slide = Maybe.withDefault (Slide "" "")
      <| get (Maybe.withDefault 0 (head deck.history)) deck.slides
    tools = [
      Component.Tools.Add [ onClick address AddSlide ]
    ]
  in
    div
    [ classList
      [ ("deck-wrapper deck-editor-wrapper", True) ]
    ]
    [ div
      [ classList
        [ ("deck-border deck-editor-border", True)
        , ("is-focused", editor.focused)
        ]
      ]
      [ div [ classList [ ("deck deck-editor", True) ] ]
        [ input
          [ classList [ ("editor-slide-title", True) ]
          , onBlur address (SetFocus False)
          , onFocus address (SetFocus True)
          , on "input" targetValue (Signal.message address << UpdateTitle)
          , value slide.title
          ] []
        , div [ classList [ ("text-area-wrapper", True) ] ]
          [ textarea
            [ classList [ ("editor-slide-body", True) ]
            , onBlur address (SetFocus False)
            , onFocus address (SetFocus True)
            , on "input" targetValue (Signal.message address << UpdateBody)
            , value slide.body
            ] []
          ]
        ]
      ]
    , div [ classList [ ("deck-tools", True)] ]
      [ (Component.Tools.view tools) ] 
    ]