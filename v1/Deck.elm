module Deck where

import Array exposing (Array, get, length, push)
import Effects exposing (..)
import Html exposing (..)
import Html.Attributes exposing (classList)
import Html.Lazy exposing (lazy)
import Http
import Json.Decode as Json exposing ((:=))
import List exposing (head, length)
import Maybe exposing (withDefault, Maybe(Just))
import Regex exposing (replace, regex)
import String exposing (toLower)
import Task exposing (Task)

import Types exposing (Deck, Slide)
import Actions exposing (DeckAction (..))
import Slide exposing (view)

idify : String -> String
idify title =
  toLower (replace Regex.All (regex "[^a-zA-Z0-9]+") (\_ -> "-") title)

current : Deck -> Int
current deck =
  withDefault 0 <| head deck.history

next : Deck -> Int
next deck =
  min ((Array.length deck.slides) - 1) <| current deck + 1

prev : Deck -> Int
prev deck =
  max 0 <| current deck - 1

-- Model

create : String -> Maybe String -> (Array Slide) -> Deck
create title author slides =
  { id = idify title
  , title = title
  , author = withDefault "" author
  , slides = slides
  , history = if Array.length slides > 0 then [0] else []
  }

-- Update

update : DeckAction -> Deck -> (Deck, Effects DeckAction)
update action deck =
  case action of
    Actions.NoOp ->
      deck

    Actions.Load title ->
      deck

    Actions.AddSlide slide ->
      {deck | slides = push slide deck.slides }

    Actions.Forward ->
      {deck | history = next deck :: deck.history}

    Actions.Backward ->
      {deck | history = prev deck :: deck.history}

-- View

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
      , div [] [ text <| List.foldr (++) "" <| List.map toString deck.history ]
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

-- Effects

get : String -> Effects DeckAction
get title =
  (fetch title)
    |> Task.toMaybe
    |> Task.map Load
    |> Effects.task

fetch : String -> Task Http.Error (Deck)
fetch title =
  Http.get decode ("http://0.0.0.0:8000/data/" ++ (idify title) ++ ".json")

decode : Json.Decoder Deck
decode =
  Json.object5 Deck
    ("id" := Json.string)
    ("title" := Json.string)
    ("author" := Json.string)
    ("slides" := Json.array
      (Json.object2 Slide
        ("title" := Json.string)
        ("body" := Json.string))
    )
    ("history" := Json.list Json.int)