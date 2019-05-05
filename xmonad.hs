-- xmonad-0.13

import           XMonad                       hiding (Tall)
import           XMonad.Hooks.DynamicLog      (dynamicLogWithPP, ppOutput,
                                               ppSep, ppSort, ppTitle,
                                               xmobarColor, xmobarPP)
import           XMonad.Hooks.EwmhDesktops    (ewmh, fullscreenEventHook)
import           XMonad.Hooks.ManageDocks     (ToggleStruts (..), avoidStruts,
                                               docks)
import           XMonad.Hooks.ManageHelpers   (doFullFloat, isDialog,
                                               isFullscreen)
import           XMonad.Hooks.UrgencyHook     (NoUrgencyHook (..),
                                               withUrgencyHook)
import           XMonad.Layout.BoringWindows  (boringWindows, focusDown,
                                               focusUp)
import           XMonad.Layout.HintedTile     (Alignment (..), HintedTile (..),
                                               Orientation (..))
import           XMonad.Layout.Maximize       (maximize, maximizeRestore)
import           XMonad.Layout.Minimize       (MinimizeMsg (..), minimize,
                                               minimizeWindow)
import           XMonad.Layout.NoBorders      (noBorders, smartBorders)
import           XMonad.Layout.Renamed        (Rename (..), renamed)
import           XMonad.Prompt                (XPConfig (..), XPPosition (..))
import           XMonad.Prompt.Shell          (shellPrompt)
import           XMonad.Util.Cursor           (setDefaultCursor)
import           XMonad.Util.EZConfig         (additionalKeysP)
import           XMonad.Util.Run              (hPutStrLn, spawnPipe,
                                               unsafeSpawn)
import           XMonad.Util.WorkspaceCompare (getSortByXineramaRule)

main = do
    myStatusBar <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
    xmonad $ docks
           $ ewmh
           $ withUrgencyHook NoUrgencyHook
           $ def { layoutHook      = myLayoutHook
                 , manageHook      = myManageHook
                 , handleEventHook = myHandleEventHook
                 , modMask         = mod4Mask
                 , logHook         = myLogHook myStatusBar
                 , startupHook     = myStartupHook
                 }
                 `additionalKeysP` myAdditionalKeysP

myLayoutHook =   (renamed [CutWordsLeft 2])
               $ smartBorders $ avoidStruts $ boringWindows $ maximize $ minimize
               $ (hintedTile Tall ||| hintedTile Wide)
             ||| (avoidStruts $ noBorders Full)
    where
        hintedTile = HintedTile nmaster delta ratio TopLeft
        nmaster    = 1
        ratio      = 1/2
        delta      = 3/100

myManageHook = composeAll [
                     isFullscreen                      --> doFullFloat
                   , isDialog                          --> doFloat
                   , className =? "mpv"                --> doFloat
                   , className =? "Gimp"               --> doFloat
                   , className =? "Thunderbird"        --> doShift "9"
                   , className =? "Slack"              --> doShift "9"
                   ]

myHandleEventHook = fullscreenEventHook

myLogHook h = dynamicLogWithPP $ xmobarPP {
                    ppSep    = " | "
                  , ppTitle  = xmobarColor "green" ""
                  , ppOutput = hPutStrLn h
                  , ppSort   = getSortByXineramaRule
                  }

myStartupHook = do
    setDefaultCursor xC_left_ptr

myAdditionalKeysP = [
      ("M-p",   shellPrompt myXPConfig)

    -- Minimize/BoringWindow-related
    , ("M-j",   focusDown)
    , ("M-k",   focusUp)
    , ("M-m",   withFocused (sendMessage . maximizeRestore))
    , ("M-n",   withFocused minimizeWindow)
    , ("M-S-n", sendMessage RestoreNextMinimizedWin)

    -- Toggle dock visibility
    , ("M-b",   sendMessage ToggleStruts)

    , ("M-S-l", unsafeSpawn "alock -auth pam -bg blank")
    , ("M-c",   unsafeSpawn "chromium --incognito")
    ]

myXPConfig = def { font              = "xft:sans-serif:size=6"
                 , bgColor           = "black"
                 , fgColor           = "grey"
                 , promptBorderWidth = 0
                 , position          = Top
                 , alwaysHighlight   = True
                 , height            = 40
                 }

