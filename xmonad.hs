import           XMonad                       hiding (Tall)
import           XMonad.Actions.Minimize      (maximizeWindowAndFocus,
                                               minimizeWindow,
                                               withLastMinimized)
import           XMonad.Hooks.EwmhDesktops    (ewmh, ewmhFullscreen)
import           XMonad.Hooks.ManageDocks     (ToggleStruts (..), avoidStruts,
                                               docks)
import           XMonad.Hooks.ManageHelpers   (doFullFloat, isDialog,
                                               isFullscreen)
import           XMonad.Hooks.StatusBar       (statusBarProp, withSB)
import           XMonad.Hooks.StatusBar.PP    (PP (..), shorten, wrap,
                                               xmobarColor, xmobarPP)
import           XMonad.Hooks.UrgencyHook     (NoUrgencyHook (..),
                                               withUrgencyHook)
import           XMonad.Layout.BoringWindows  (boringWindows, focusDown,
                                               focusUp)
import           XMonad.Layout.HintedTile     (Alignment (..), HintedTile (..),
                                               Orientation (..))
import           XMonad.Layout.Maximize       (maximize, maximizeRestore)
import           XMonad.Layout.Minimize       (minimize)
import           XMonad.Layout.NoBorders      (noBorders, smartBorders)
import           XMonad.Layout.Renamed        (Rename (..), renamed)
import           XMonad.Prompt                (XPConfig (..), XPPosition (..))
import           XMonad.Prompt.Shell          (shellPrompt)
import           XMonad.Util.Cursor           (setDefaultCursor)
import           XMonad.Util.EZConfig         (additionalKeysP)
import           XMonad.Util.Run              (safeSpawn)
import           XMonad.Util.WorkspaceCompare (getSortByXineramaRule)

main = do
    xmonad . ewmhFullscreen . ewmh
           . withUrgencyHook NoUrgencyHook
           . withSB myStatusBar
           . docks
           $ def { layoutHook  = myLayoutHook
                 , manageHook  = myManageHook
                 , modMask     = mod4Mask
                 , startupHook = myStartupHook
                 }
                 `additionalKeysP` myAdditionalKeysP

myStatusBar = statusBarProp "xmobar --dpi 254 $HOME/.config/xmonad/xmobarrc" (pure myXmobarPP)

myXmobarPP = xmobarPP {
      ppVisible = grey . wrap "(" ")"
    , ppHidden  = grey
    , ppSep     = " | "
    , ppTitle   = xmobarColor "#00cc00" "" . shorten 80
    , ppLayout  = grey
    , ppSort    = getSortByXineramaRule
    }
  where
    grey = xmobarColor "#999999" ""

myLayoutHook =
    (renamed [CutWordsLeft 2])
    . smartBorders . avoidStruts . boringWindows . maximize . minimize
    $ (hintedTile Tall ||| hintedTile Wide ||| Full)
  where
    hintedTile = HintedTile nmaster delta ratio TopLeft
    nmaster    = 1
    ratio      = 1/2
    delta      = 3/100

myManageHook = composeAll [
      isFullscreen        --> doFullFloat
    , isDialog            --> doFloat
    , className =? "mpv"  --> doFloat
    , className =? "Gimp" --> doFloat
    ]

myStartupHook = do
    setDefaultCursor xC_left_ptr

myAdditionalKeysP = [
      ("M-p",   shellPrompt myXPConfig)

    -- Minimize/BoringWindow-related
    , ("M-j",   focusDown)
    , ("M-k",   focusUp)
    , ("M-m",   withFocused (sendMessage . maximizeRestore))
    , ("M-n",   withFocused minimizeWindow)
    , ("M-S-n", withLastMinimized maximizeWindowAndFocus)

    -- Toggle dock visibility
    , ("M-b",   sendMessage ToggleStruts)

    , ("M-c",   safeSpawn "chromium" ["--incognito", "--force-device-scale-factor=1.8"])
    ]

myXPConfig = def {
      font              = "xft:Noto Sans Mono:size=6"
    , bgColor           = "black"
    , fgColor           = "grey"
    , promptBorderWidth = 0
    , position          = Top
    , alwaysHighlight   = True
    , height            = 34
    }
