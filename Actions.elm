module Actions where

import Types exposing (Slide)

type DeckAction =
  NoOp
  | Load String
  | AddSlide Slide
  | Forward
  | Backward