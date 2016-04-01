module Actions where

import Types exposing (Slide, Deck)

type DeckAction =
  NoOp
  | Load (Deck)
  | AddSlide Slide
  | Forward
  | Backward