{-# LANGUAGE TypeApplications #-}

module UI
  ( app,
    CompilationEvent (..),
  )
where

import Brick
import Compilation (compile)
import qualified Graphics.Vty as Vty
import Numeric (showFFloat)
import RIO
import qualified RIO.Text as Text
import qualified RIO.Time as Time
import Types

data CompilationEvent
  = CompilationSucceeded SuccessfulCompilation
  | CompilationFailed FailedCompilation
  deriving (Eq, Show, Generic)

data Name = Gotyno
  deriving (Eq, Ord)

app :: App CompilationState CompilationEvent Name
app =
  App
    { appDraw = drawCompilationState,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const attrMap'
    }

successAttr :: AttrName
successAttr = "successAttr"

failureAttr :: AttrName
failureAttr = "failureAttr"

attrMap' :: AttrMap
attrMap' =
  attrMap Vty.defAttr [(successAttr, fg Vty.green), (failureAttr, fg Vty.red)]

drawCompilationState :: CompilationState -> [Widget Name]
drawCompilationState CompilationState {state = Right compilation} =
  [drawSuccessfulCompilation compilation]
drawCompilationState CompilationState {state = Left compilation} =
  [drawFailedCompilation compilation]

drawSuccessfulCompilation :: SuccessfulCompilation -> Widget Name
drawSuccessfulCompilation
  SuccessfulCompilation
    { totalTime,
      parsingTime,
      languageTimes,
      moduleStatistics
    } =
    [ drawLanguageTimes languageTimes,
      drawAllModuleStatistics moduleStatistics,
      str "Parsing time: " <+> str (showFFloat @Float (Just 3) (realToFrac parsingTime) "s"),
      str "Total time: " <+> str (showFFloat @Float (Just 3) (realToFrac totalTime) "s")
    ]
      & map (withAttr successAttr)
      & vBox

drawLanguageTimes :: LanguageOutputStatistics -> Widget Name
drawLanguageTimes LanguageOutputStatistics {typescriptTime, fsharpTime, pythonTime} =
  let tsWidget = maybe emptyWidget (drawTimeOutput "TypeScript output") typescriptTime
      fsWidget = maybe emptyWidget (drawTimeOutput "F# output") fsharpTime
      pyWidget = maybe emptyWidget (drawTimeOutput "Python output") pythonTime
   in vBox [tsWidget, fsWidget, pyWidget]

drawAllModuleStatistics :: [ModuleStatistics] -> Widget Name
drawAllModuleStatistics = fmap drawModuleStatistics >>> vBox

drawModuleStatistics :: ModuleStatistics -> Widget Name
drawModuleStatistics ModuleStatistics {name, time, language} = do
  let moduleOutput = Text.unpack name <> " (" <> showOutputLanguage language <> ")" <> ": "
      timeOutput = showFFloat @Double (Just 4) (realToFrac time) "s"
  [ padLeft (Pad 4) $
      str moduleOutput <+> str timeOutput
    ]
    & map (withAttr successAttr)
    & vBox

drawTimeOutput :: String -> Time.NominalDiffTime -> Widget Name
drawTimeOutput label time =
  str (label <> ": ") <+> str (showFFloat @Float (Just 2) (realToFrac time) "s")

drawFailedCompilation :: FailedCompilation -> Widget Name
drawFailedCompilation (FailedCompilation errors) =
  vBox
    [ withAttr failureAttr $ str "Compilation failed:",
      withAttr failureAttr $ padLeft (Pad 2) $ vBox (map drawError errors)
    ]

drawError :: String -> Widget Name
drawError = str >>> withAttr failureAttr

handleEvent ::
  CompilationState ->
  BrickEvent Name CompilationEvent ->
  EventM Name (Next CompilationState)
handleEvent s (AppEvent (CompilationSucceeded succeeded)) =
  continue $ s {state = Right succeeded}
handleEvent s (AppEvent (CompilationFailed failed)) =
  continue $ s {state = Left failed}
handleEvent state (VtyEvent (Vty.EvKey Vty.KEsc [])) = halt state
handleEvent state (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt state
handleEvent CompilationState {options} (VtyEvent (Vty.EvKey (Vty.KChar 'r') [])) = do
  newCompilationState <- liftIO $ compile options
  continue newCompilationState
handleEvent state _ = continue state

showOutputLanguage :: OutputLanguage -> String
showOutputLanguage TypeScriptOutput = "TypeScript"
showOutputLanguage FSharpOutput = "F#"
showOutputLanguage PythonOutput = "Python"
