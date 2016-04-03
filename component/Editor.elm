module Component.Editor where

import Array exposing (get)
import List exposing (head)
import Maybe
import Html exposing (..)
import Html.Attributes exposing (classList, property, value)
import Html.Events exposing (on, onFocus, onBlur, targetValue)
import Json.Encode exposing (string)
import Maybe

import Component.Deck exposing (Deck)
import Component.Slide exposing (Slide)
import ElmDeck exposing (..)

-- Model

type alias Editor =
  { editing : EditingState
  , focused : FocusState
  }

-- Actions

type Action =
  SetFocus FocusState
  | ToggleEditing
  | UpdateTitle String
  | UpdateBody String

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

onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address contentToValue =
    on "input" targetValue (\str -> Signal.message address (contentToValue str))

view: Signal.Address Action -> (Editor, Deck) -> Html
view address (editor, deck) =
  let
    slide = Maybe.withDefault (Slide "" "") <| get (Maybe.withDefault 0 (head deck.history)) deck.slides
  in
    div
    [ classList
      [ ("deck-wrapper deck-editor-wrapper", True)
      , ("is-editing", editor.editing)
      ]
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
          , onInput address UpdateTitle
          , property "type" (string "text")
          , value slide.title
          ] []
        , textarea 
          [ classList [ ("editor-slide-body", True) ]
          , onBlur address (SetFocus False)
          , onFocus address (SetFocus True)
          , onInput address UpdateBody
          , value slide.body
          ] []
        ]
      ]
    ]