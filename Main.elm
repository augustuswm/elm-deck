import Array exposing (fromList)
import Html exposing (..)
import Maybe exposing (Maybe(Just))

import Deck exposing (create, view)
import Slide exposing (create)

main =
  Deck.view (Deck.create "My New Deck" "amayo" (fromList [Slide.create "The First Title" (Just "#A Slide Header")]))