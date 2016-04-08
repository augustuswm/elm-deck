module Component.Deck where

import Array exposing (Array, append, fromList, get, push, slice)
import Effects exposing (..)
import Html exposing (..)
import Html.Attributes exposing (classList)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Json exposing ((:=))
import List exposing (head, length)
import Maybe
import Regex exposing (replace, regex)
import String exposing (toLower)
import Task

import Component.Slide exposing (Slide)
import Component.Tools exposing(Tools)
import ElmDeck exposing (..)

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
  | AddSlide
  | Forward
  | Backward
  | SlideAction Component.Slide.Action

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

insertBlankSlide : Int -> Array Slide -> Array Slide
insertBlankSlide n slides =
  append (push (Slide "" "") <| slice 0 n slides) <| slice n (Array.length slides) slides

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

    AddSlide ->
      let
        newSlides = insertBlankSlide (next deck) deck.slides
      in  
        ( {deck
          | slides = newSlides
          , history = next deck :: deck.history
          }
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

    SlideAction slideAction ->
      let
        currentPos = Maybe.withDefault 0 <| head deck.history
        newSlides = Array.set currentPos (Component.Slide.update slideAction (get currentPos deck.slides)) deck.slides
      in
        ( { deck | slides = newSlides }
        , Effects.none
        )

-- View

view : Signal.Address Action -> (Deck) -> Html
view address (deck) =
  let
    slide = (Array.get (Maybe.withDefault 0 (head deck.history)) deck.slides)
    tools = [
      Component.Tools.Backward [ onClick address Backward ],
      Component.Tools.Forward [ onClick address Forward ]
    ]
  in  
    div [ classList [ ("deck-wrapper", True) ] ]
    [ div [ classList [ ("deck-border", True) ] ]
      [ div [ classList [ ("deck", True) ] ]
        [ div [ classList [ ("deck-header", True) ] ]
          [ small [ classList [ ("deck-header-title", True) ] ] [ text deck.title ]
          , small [ classList [ ("deck-header-author", True) ] ] [ text deck.author ]
          ]
        , div [ classList [ ("slide-container", True) ] ]
          [ Component.Slide.view slide ]
        , div [ classList [ ("slide-extra-info", True) ] ]
          [ div [ classList [ ("slide-title", True) ] ]
            [ small []
              [ text <| (Maybe.withDefault (Slide "" "") slide).title ]
            ]
          , div [ classList [ ("slide-position", True) ] ]
            [ small []
              [ text <| toString <| (Maybe.withDefault 0 <| head deck.history) + 1
              , text "/"
              , text <| toString <| Array.length deck.slides
              ]
            ]
          ]
        ]
      ]
    , div [ classList [ ("deck-tools", True) ] ]
      [ (Component.Tools.view tools) ]
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
        ("body" := Json.string)
      )
    )
    ("history" := Json.list Json.int)