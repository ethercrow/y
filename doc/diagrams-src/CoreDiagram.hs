module Main
    ( main
    ) where

import Diagrams.Prelude hiding (snapshot)
import Diagrams.Backend.SVG.CmdLine

coreDiagram :: Diagram B R2
coreDiagram = hcat [graph, legend]

graph :: Diagram B R2
graph = applyAll arrows .  hcat' with {_sep = 2} $
    [ ioStuff
    , vcat' with {_sep = 2} . fmap arrangeInCircle $
        [coreInputs, coreActionStuff, coreStateStuff, coreOutputs]
    ]

ioStuff :: Diagram B R2
ioStuff = (text "IO" <> circle 1) # scale 3 # named "IO"

arrangeInCircle :: [Diagram B R2] -> Diagram B R2
arrangeInCircle ds = decorateTrail (regPoly (length ds) 7) ds

coreInputs :: [Diagram B R2]
coreInputs =
    [ event "startupAction" # named "e-startupAction"
    , event "input" # named "e-input"
    ]

coreOutputs :: [Diagram B R2]
coreOutputs = [event "output" # named "e-output"]

coreActionStuff :: [Diagram B R2]
coreActionStuff =
    [ event "textUpdates" # named "e-textUpdates"
    , event "configMod" # named "e-configMod"
    , behavior "config" # named "b-config"
    , snapshot # named "s-action"
    , merge # named "m-action"
    , event "asyncAction" # named "e-asyncAction"
    , event "action" # named "e-action"
    , event "syncAction" # named "e-syncAction"
    , event "highlightAction" # named "e-highlighterAction"
    ]

coreStateStuff :: [Diagram B R2]
coreStateStuff =
    [ behavior "state" # named "b-state"
    , event "stateMod" # named "e-stateMod"
    , merge # named "m-output"
    ]

legend :: Diagram B R2
legend = vcat' with {_sep = 0.3}
    $ fmap (uncurry (|||) . fmap textBox)
    [ (merge, "merge")
    , (snapshot, "snapshot")
    ] ++
    [ event "Event", behavior "Behavior"]

arrows :: [Diagram B R2 -> Diagram B R2]
arrows = fmap (uncurry (connectOutside' arrowOpts)) . concat $
    [ concatMap mergeArrows
                [ (["b-state", "e-syncAction"],
                    "m-output", "e-output")
                , (["e-startupAction", "e-asyncAction", "e-highlighterAction", "s-action"],
                    "m-action", "e-action")
                ]
    , snapshotArrows ("e-input", "b-config", "s-action", "m-action")
    , [ ("e-action", "e-syncAction")
      , ("e-action", "e-asyncAction")
      , ("e-stateMod", "b-state")
      , ("e-configMod", "b-config")
      , ("e-syncAction", "e-stateMod")
      , ("e-syncAction", "e-configMod")
      , ("b-state", "e-textUpdates")
      , ("e-textUpdates", "e-highlighterAction")
      , ("e-asyncAction", "IO")
      , ("IO", "e-asyncAction")
      ]
    ]

snapshotArrows :: (String, String, String, String) -> [(String, String)]
snapshotArrows (inputEvent, modBehavior, snapshotNode, outputEvent)
    = [ (inputEvent, snapshotNode)
      , (modBehavior, snapshotNode)
      , (snapshotNode, outputEvent)
      ]

mergeArrows :: ([String], String, String) -> [(String, String)]
mergeArrows (inputs, merger, output)
    = (merger, output) : [(input, merger) | input <- inputs]

event :: String -> Diagram B R2
event = textBox

behavior :: String -> Diagram B R2
behavior s = text s <> circle 0.5 # scale (guessTextSize s) 

merge :: Diagram B R2
merge = circle 0.3 # fc orange

snapshot :: Diagram B R2
snapshot = circle 0.3 # fc blue

textBox :: String -> Diagram B R2
textBox s = text s <> rect (w + 1) (w / 2)
    where w = guessTextSize s

guessTextSize :: String -> Double
guessTextSize = (/ 2) . fromIntegral . length

arrowOpts :: ArrowOpts
arrowOpts = with & headGap .~ 0.2
                 & tailGap .~ 0.2
                 & headSize .~ 1.0

main :: IO ()
main = mainWith coreDiagram
