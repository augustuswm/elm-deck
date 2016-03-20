module Deck where

import Array exposing (Array, get, length, push)
import Html exposing (..)
import Html.Attributes exposing (classList)
import Html.Lazy exposing (lazy)
import List exposing (head, length)
import Maybe exposing (withDefault)
import Regex exposing (replace, regex)
import String exposing (toLower)

import Types exposing (Deck, Slide)
import Actions exposing (DeckAction)
import Slide exposing (view)

idify : String -> String
idify title =
  toLower (replace Regex.All (regex "[^a-zA-Z0-9]+") (\_ -> "-") title)

create : String -> String -> (Array Slide) -> Deck
create title author slides =
  { id = idify title
  , title = title
  , author = author
  , slides = slides
  , history = []
  }

load : String -> Maybe String -> (Array Slide) -> Deck
load title author slides =
  case author of
    Just value -> create title value slides
    Nothing -> create title "" slides

update : DeckAction -> Deck -> Deck
update action deck =
  case action of
    Actions.NoOp ->
      deck

    Actions.Load title ->
      deck

    Actions.AddSlide slide ->
      {deck | slides = push slide deck.slides }

    Actions.Forward ->
      {deck | history = (min (Array.length deck.slides) (withDefault 0 (head deck.history)) + 1) :: deck.history}

    Actions.Backward ->
      {deck | history = (max (Array.length deck.slides) (withDefault 0 (head deck.history)) - 1) :: deck.history}

view : Deck -> Html
view deck =
  div
    [ classList [
        ("deck", True)
      ]
    ]
    [ div
      [ classList [
          ("deck-header", True)
        ]
      ]
      [
        div [] [ text deck.title ]
      , div [] [ text deck.author ]
      ]
    , div
      [ classList [
          ("slide-container", True)
        ]
      ]
      [
        Slide.view (get (withDefault 0 (head deck.history)) deck.slides)
      ]
    ]