module Slide where

import Html exposing (Html, text)
import Markdown exposing (toHtml)

import Types exposing (Slide)

create : String -> Maybe String -> Slide
create title body =
  case body of
    Just value ->
      { title = title
      , body = value
      }
    Nothing ->
      { title = title
      , body = ""
      }

view : Maybe Slide -> Html
view slide =
  case slide of
    Just value -> toHtml value.body
    Nothing -> text ""
