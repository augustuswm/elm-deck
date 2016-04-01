module Component.Slide where

import Html exposing (Html, text)
import Markdown exposing (toHtml)

type alias Slide =
  { title : String
  , body : String
  }

view : Maybe Slide -> Html
view slide =
  case slide of
    Just value -> toHtml value.body
    Nothing -> text ""
