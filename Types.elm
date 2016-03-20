module Types where

import Array exposing (Array)

type alias Deck =
  { id : String
  , title : String
  , author : String
  , slides : Array Slide
  , history : List Int
  }

type alias Slide =
  { title : String
  , body : String
  }