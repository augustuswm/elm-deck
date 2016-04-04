module Component.Tools where

import Html exposing (..)
import Html.Attributes exposing (classList, href, title)
import List exposing (append, map)

type alias Tools = List Tool

type Tool =
  Add (List Html.Attribute)
  | Edit (List Html.Attribute) 
  | Delete (List Html.Attribute)
  | Fullscreen (List Html.Attribute)
  | Forward (List Html.Attribute)
  | Backward (List Html.Attribute)

-- View

viewTool : Tool -> Html
viewTool tool =
  case tool of
    Add attributes ->
      a (append [ href "#", classList [ ("sub-control-add", True) ] ] attributes)
      [ i [ title "Add", classList [ ("fa fa-plus", True) ] ] [] ]
    
    Edit attributes ->
      a (append [ href "#", classList [ ("sub-control-edit", True) ] ] attributes)
      [ i [ title "Edit", classList [ ("fa fa-pencil", True) ] ] [] ]
    
    Delete attributes ->
     a (append [ href "#", classList [ ("sub-control-delete", True) ] ] attributes)
     [ i [ title "Delete", classList [ ("fa fa-times", True) ] ] [] ]
  
    Fullscreen attributes ->
      a (append [ href "#", classList [ ("sub-control-fullscreen", True) ] ] attributes)
      [ i [ title "Fullscreen", classList [ ("fa fa-arrows-alt", True) ] ] [] ]  

    Forward attributes ->
      a (append [ href "#", classList [ ("sub-control-forward", True) ] ] attributes)
      [ i [ title "Forward", classList [ ("fa fa-angle-right", True) ] ] [] ]

    Backward attributes ->
      a (append [ href "#", classList [ ("sub-control-backward", True) ] ] attributes)
      [ i [ title "Backward", classList [ ("fa fa-angle-left", True) ] ] [] ]
 
view : Tools -> Html
view tools =
  ul [ classList [ ("sub-controls", True) ] ]
  <| map (\x -> li [ classList [ ("sub-control", True) ] ] [ x ]) <| map viewTool tools