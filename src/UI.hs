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
import Qtility
import qualified RIO.Text as Text
import qualified RIO.Time as Time
import Types

data CompilationEvent
  = CompilationSucceeded !SuccessfulCompilation
  | CompilationFailed !FailedCompilation
  deriving (Eq, Show, Generic)

data Name = Gotyno
  deriving (Eq, Ord)

app :: App CompilationState CompilationEvent Name
app =
  App
    { appDraw = drawApplication,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = pure,
      appAttrMap = const attrMap'
    }

successAttr :: AttrName
successAttr = "successAttr"

failureAttr :: AttrName
failureAttr = "failureAttr"

attrMap' :: AttrMap
attrMap' =
  attrMap Vty.defAttr [(successAttr, fg Vty.green), (failureAttr, fg Vty.red)]

drawApplication :: CompilationState -> [Widget Name]
drawApplication CompilationState {_compilationStateState = Right compilation} =
  [drawSuccessfulCompilation compilation]
drawApplication CompilationState {_compilationStateState = Left compilation} =
  [drawFailedCompilation compilation]

drawSuccessfulCompilation :: SuccessfulCompilation -> Widget Name
drawSuccessfulCompilation
  SuccessfulCompilation
    { _successfulCompilationTotalTime = totalTime,
      _successfulCompilationParsingTime = parsingTime,
      _successfulCompilationLanguageTimes = languageTimes,
      _successfulCompilationModuleStatistics = moduleStatistics
    } =
    [ drawLanguageTimes languageTimes,
      drawAllModuleStatistics moduleStatistics,
      str "Parsing time: " <+> str (showFFloat @Float (Just 3) (realToFrac parsingTime) "s"),
      str "Total time: " <+> str (showFFloat @Float (Just 3) (realToFrac totalTime) "s")
    ]
      & map (withAttr successAttr)
      & vBox

drawLanguageTimes :: LanguageOutputStatistics -> Widget Name
drawLanguageTimes stats =
  let tsWidget =
        maybe emptyWidget (drawTimeOutput "TypeScript output") $
          stats ^. languageOutputStatisticsTypescriptTime
      fsWidget =
        maybe emptyWidget (drawTimeOutput "F# output") $
          stats ^. languageOutputStatisticsFsharpTime
      pyWidget =
        maybe emptyWidget (drawTimeOutput "Python output") $
          stats ^. languageOutputStatisticsPythonTime
      hsWidget =
        maybe emptyWidget (drawTimeOutput "Haskell output") $
          stats ^. languageOutputStatisticsHaskellTime
      ktWidget =
        maybe emptyWidget (drawTimeOutput "Kotlin output") $
          stats ^. languageOutputStatisticsKotlinTime
   in vBox [tsWidget, fsWidget, pyWidget, hsWidget, ktWidget]

drawAllModuleStatistics :: [ModuleStatistics] -> Widget Name
drawAllModuleStatistics = fmap drawModuleStatistics >>> vBox

drawModuleStatistics :: ModuleStatistics -> Widget Name
drawModuleStatistics
  ModuleStatistics
    { _moduleStatisticsName = name,
      _moduleStatisticsTime = time,
      _moduleStatisticsLanguage = language
    } = do
    let moduleOutput = Text.unpack name <> " (" <> showOutputLanguage language <> "): "
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
  [ str "Compilation failed:",
    padLeft (Pad 2) $ vBox (map str errors)
  ]
    & map (withAttr failureAttr)
    & vBox

handleEvent ::
  CompilationState ->
  BrickEvent Name CompilationEvent ->
  EventM Name (Next CompilationState)
handleEvent s (AppEvent (CompilationSucceeded succeeded)) =
  continue $ s & compilationStateState .~ Right succeeded
handleEvent s (AppEvent (CompilationFailed failed)) =
  continue $ s & compilationStateState .~ Left failed
handleEvent state (VtyEvent (Vty.EvKey Vty.KEsc [])) = halt state
handleEvent state (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt state
handleEvent s (VtyEvent (Vty.EvKey (Vty.KChar 'r') [])) = do
  newCompilationState <- liftIO $ compile $ s ^. compilationStateOptions
  continue newCompilationState
handleEvent state (VtyEvent _) = continue state
handleEvent state MouseDown {} = continue state
handleEvent state MouseUp {} = continue state

showOutputLanguage :: OutputLanguage -> String
showOutputLanguage TypeScriptOutput = "TypeScript"
showOutputLanguage FSharpOutput = "F#"
showOutputLanguage PythonOutput = "Python"
showOutputLanguage HaskellOutput = "Haskell"
showOutputLanguage KotlinOutput = "Kotlin"
