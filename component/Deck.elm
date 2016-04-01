module Component.Deck where

import Array exposing (Array, fromList)
import Effects exposing (..)
import Html exposing (..)
import Html.Attributes exposing (classList)
import Http
import Json.Decode as Json exposing ((:=))
import List exposing (head, length)
import Maybe
import Regex exposing (replace, regex)
import String exposing (toLower)
import Task

import Component.Slide exposing (Slide)

-- Model

type alias Deck =
  { id : String
  , title : String
  , author : String
  , slides : Array Slide
  , history : List Int
  }

-- Actions

type Action =
  NoOp
  | New String
  | Fetch String
  | Load (Maybe Deck)
  | Forward
  | Backward

-- Utilities

idify : String -> String
idify title =
  toLower (replace Regex.All (regex "[^a-zA-Z0-9]+") (\_ -> "-") title)

url : String -> String
url idifiedTitle =
  "http://0.0.0.0:8000/data/" ++ idifiedTitle ++ ".json"

current : Deck -> Int
current deck =
  Maybe.withDefault 0 <| head deck.history

next : Deck -> Int
next deck =
  min ((Array.length deck.slides) - 1) <| current deck + 1

prev : Deck -> Int
prev deck =
  max 0 <| current deck - 1

-- Initialize

init : String -> (Deck, Effects Action)
init title =
  ( Deck (idify title) title "" (fromList []) []
  , load (idify title)
  )

-- Update

update : Action -> Deck -> (Deck, Effects Action)
update action deck =
  case action of
    NoOp ->
      ( deck
      , Effects.none
      )

    New title ->
      init title

    Fetch title ->
      ( deck
      , load (idify title)
      )

    Load maybeDeck ->
      ( Maybe.withDefault deck maybeDeck
      , Effects.none
      )

    Forward ->
      ( {deck | history = next deck :: deck.history}
      , Effects.none
      )

    Backward ->
      ( {deck | history = prev deck :: deck.history}
      , Effects.none
      )

-- View

view : Signal.Address Action -> Deck -> Html
view address deck =
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
        Component.Slide.view (Array.get (Maybe.withDefault 0 (head deck.history)) deck.slides)
      ]
    ]

-- Effects

load : String -> Effects Action
load idifiedTitle =
  Http.get decode (url idifiedTitle)
    |> Task.toMaybe
    |> Task.map Load
    |> Effects.task

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