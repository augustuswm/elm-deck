module Component.Slide where

import Html exposing (Html, text)
import Markdown exposing (toHtml)

-- Model

type alias Slide =
  { title : String
  , body : String
  }

-- Actions

type Action =
  UpdateTitle String
  | UpdateBody String

-- Update

update : Action -> (Maybe Slide) -> Slide
update action slide =
  case action of
    UpdateTitle title ->
      case slide of
        Just value -> { value | title = title }
        Nothing -> Slide title ""

    UpdateBody body ->
      case slide of
        Just value -> { value | body = body }
        Nothing -> Slide "" body

-- View

view : Maybe Slide -> Html
view slide =
  case slide of
    Just value -> toHtml value.body
    Nothing -> text ""
