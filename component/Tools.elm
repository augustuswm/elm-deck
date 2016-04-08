module Component.Tools where

import Html exposing (..)
import Html.Attributes exposing (classList, href, title)
import List exposing (append, map)

type alias Tools = List Tool

type Tool =
  Add (List Html.Attribute)
  | Edit (List Html.Attribute) Bool
  | Delete (List Html.Attribute)
  | Fullscreen (List Html.Attribute) Bool
  | Forward (List Html.Attribute)
  | Backward (List Html.Attribute)
  | Theme (List Html.Attribute) Bool

-- View

defafultAttributes : String -> Bool -> (List Html.Attribute)
defafultAttributes class active =
  [ href "#"
  , classList [ ("sub-action", True), (class, True), ("active", active) ]
  ]

mergeAttributes : String -> Bool -> (List Html.Attribute) -> (List Html.Attribute)
mergeAttributes class active attributes =
  (append (defafultAttributes class active) attributes)

makeTool : String -> String -> (List Html.Attribute) -> Html
makeTool toolTitle icon attributes =
  a attributes
  [ i
    [ title toolTitle
    , classList [ ("fa", True), ("fa-" ++ icon, True) ] 
    ] []
  ]

viewTool : Tool -> Html
viewTool tool =
  case tool of
    Add attributes ->
      makeTool "Add" "plus"
        <| mergeAttributes "sub-control-add" False attributes
    
    Edit attributes active ->
      makeTool "Edit" "pencil"
        <| mergeAttributes "sub-control-edit" active attributes
    
    Delete attributes ->
      makeTool "Delete" "times"
        <| mergeAttributes "sub-control-delete" False attributes
  
    Fullscreen attributes active ->
      makeTool "Fullscreen" "arrows-alt"
        <| mergeAttributes "sub-control-fullscreen" active attributes  

    Forward attributes ->
      makeTool "Forward" "angle-right"
        <| mergeAttributes "sub-control-forward" False attributes

    Backward attributes ->
      makeTool "Backward" "angle-left"
        <| mergeAttributes "sub-control-backward" False attributes

    Theme attributes active ->
      makeTool "Theme" "paint-brush"
        <| mergeAttributes "sub-control-theme" active attributes      
 
view : Tools -> Html
view tools =
  ul [ classList [ ("sub-controls", True) ] ]
  <| map (\x -> li [ classList [ ("sub-control", True) ] ] [ x ]) <| map viewTool tools