import Effects exposing (Never)
import Keyboard
import StartApp
import Task

import Component.Deck exposing (Action (..), init, update, view)

app =
  StartApp.start
    { init = init "My New Deck"
    , update = update
    , view = view
    , inputs = [ traverse ]
    }

-- Signals

traverse : Signal Component.Deck.Action
traverse =
  let
    keyToAction key =
      case key of
        100 ->
          Forward
        97 ->
          Backward
        _ ->
          NoOp
  in
    Signal.map keyToAction Keyboard.presses

main =
  app.html

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks