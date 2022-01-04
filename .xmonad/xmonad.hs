import XMonad hiding ((|||))
import System.IO
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified Data.ByteString as B
import qualified Codec.Binary.UTF8.String as UTF8
import qualified DBus as D
import qualified DBus.Client as D

import Control.Monad (liftM2)
import Graphics.X11.ExtraTypes.XF86

import XMonad.ManageHook
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Util.Replace

import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers(doFullFloat, doCenterFloat, isFullscreen, isDialog)

import XMonad.Actions.MouseResize
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.CycleWS

import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Spiral
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Accordion
import XMonad.Layout.WindowNavigation
import XMonad.Layout.LimitWindows
import XMonad.Layout.SimplestFloat
import XMonad.Layout.LayoutCombinators -- hiding ((|||))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import Data.Maybe (isJust, fromJust)

import XMonad.Actions.GridSelect

myTerminal         = "alacritty"
myFallbackTerminal = "cool-retro-term"
myLauncher         = "rofi -modi run,drun -show drun -show-icons"
myLauncher2        = "dmenu_run -p \"Run: \" -fn \"xft:Fira Code Retina\" -nb \"#23272e\" -nf \"#eceff4\" -sb \"#88c0d0\" -sf \"#23272e\""
myFileManager      = "thunar"
myBrowser          = "google-chrome-stable"
myEditor           = "emacsclient -c" -- maybe someday I can change it to vim, but I don't think so
emacs              = "emacsclient -c"
emacsExec          = "emacsclient --eval "

myBorderWidth   = 2
myGaps          = 2

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myModMask       = mod4Mask

myWorkspaces    = map show [1..10]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..]

myNormalBorderColor  = "#2e3440"
myFocusedBorderColor = "#88c0d0"

myFont = "xft:JetBrainsMono:style=Regular"

myTabTheme = def { fontName            = myFont
                 , activeColor         = "#2e3440"
                 , inactiveColor       = "#2e3440"
                 , activeBorderColor   = "#88c0d0"
                 , inactiveBorderColor = "#2e3440"
                 , activeTextColor     = "#eceff4"
                 , inactiveTextColor   = "#eceff4"
                 }

myColorizer = colorRangeFromClassName
              (0x2e,0x34,0x40) -- lowest  inactive bg #2e3440
              (0x2e,0x34,0x40) -- highest inactive bg #2e3440
              (0xb4,0x8d,0xad) -- active bg           #b48dad
              (0x88,0xc0,0xd0) -- inactive fg         #88c0d0
              (0x28,0x2c,0x34) -- active fg           #2e3440

myGridNavigationKey = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
 where navKeyMap = M.fromList [
          ((0,xK_Escape), cancel)
         ,((0,xK_Return), select)
         ,((0,xK_slash) , substringSearch myGridNavigationKey)
         ,((0,xK_Left)  , move (-1,0)  >> myGridNavigationKey)
         ,((0,xK_h)     , move (-1,0)  >> myGridNavigationKey)
         ,((0,xK_Right) , move (1,0)   >> myGridNavigationKey)
         ,((0,xK_l)     , move (1,0)   >> myGridNavigationKey)
         ,((0,xK_Down)  , move (0,1)   >> myGridNavigationKey)
         ,((0,xK_j)     , move (0,1)   >> myGridNavigationKey)
         ,((0,xK_Up)    , move (0,-1)  >> myGridNavigationKey)
         ,((0,xK_k)    , move (0,-1)  >> myGridNavigationKey)
         ,((0,xK_space) , setPos (0,0) >> myGridNavigationKey)
         ]
       -- The navigation handler ignores unknown key symbols
       navDefaultHandler = const myGridNavigationKey

spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    where conf = def
                   { gs_cellheight   = 40
                   , gs_cellwidth    = 200
                   , gs_cellpadding  = 6
                   , gs_originFractX = 0.5
                   , gs_originFractY = 0.5
                   , gs_font         = myFont
                   }


myGridConfig colorizer = (buildDefaultGSConfig myColorizer)
    { gs_cellheight   = 40
    , gs_cellwidth    = 200
    , gs_cellpadding  = 6
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = myFont
    , gs_navigate     = myGridNavigationKey
    }

mySysGrid = [ ("Emacs", "emacsclient -c -a emacs")
                 , ("Update Arch", "alacritty -t update-arch -e sudo pacman -Syu")
                 , ("Update AUR", "alacritty -t update-arch -e yay -Syu")
                 , ("Topgrade", "alacritty -t update-arch -e topgrade")
                 , ("XMonad Config", emacsExec ++ "'(dired \"~/.xmonad\")'")
                 , ("Emacs Config", emacsExec ++ "'(dired \"~/.config/doom\")'")
                 ]

myAppGrid = [ ("Emacs", "emacsclient -c -a emacs")
                 , ("Vim", "alacritty -e vim")
                 , ("Google", "google-chrome-stable")
                 , ("Spotify", "spotify")
                 , ("Teams", "teams")
                 , ("Telegram", "telegram-desktop")
                 , ("File Manager", myFileManager)
                 , ("Terminal", myTerminal)
                 , ("Cool Terminal", myFallbackTerminal)
                 , ("Color Picker", "kcolorchooser")
                 , ("PDF reader", "okular")
                 ]

myKeys conf@(XConfig {XMonad.modMask = modKey}) = M.fromList $
    [((m .|. modKey, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

myAdditionalKeys = [ -- Basic keybindings
                     ("M-<Return>"  , spawn $ myTerminal)
                   , ("M-S-t"       , spawn $ myFallbackTerminal)
                   , ("M1-C-t"      , spawn $ myFallbackTerminal)
                   , ("M-d"         , spawn myLauncher)
                   , ("M-S-d"       , spawn myLauncher2)
                   , ("M-w"         , spawn myBrowser)
                   , ("M-v"         , spawn "pavucontrol")
                   , ("M-S-<Return>", spawn myFileManager)
                   , ("M-S-q"       , kill)
                   , ("M-C-S-q"     , killAll)
                   , ("M-<Space>"   , sendMessage NextLayout)
                   , ("M-n"         , refresh)
                   , ("M-<Tab>"     , windows W.focusDown)
                   , ("M-S-<Tab>"   , windows W.focusUp)
                   , ("M1-<Tab>"    , windows W.focusDown)
                   , ("M1-S-<Tab>"  , windows W.focusUp)
                   , ("M-j"         , windows W.focusDown)
                   , ("M-k"         , windows W.focusUp)
                   , ("M-m"         , windows W.focusMaster)
                   , ("M-C-<Return>", windows W.swapMaster)
                   , ("M-S-j"       , windows W.swapDown)
                   , ("M-S-k"       , windows W.swapUp)
                   , ("M-h"         , sendMessage Shrink)
                   , ("M-l"         , sendMessage Expand)
                   , ("M-t"         , withFocused $ windows . W.sink)
                   , ("M-,"         , prevWS)
                   , ("M-."         , nextWS)
                   , ("M-S-,"       , prevScreen)
                   , ("M-S-."       , nextScreen)
                   , ("M-f"         , sendMessage $ Toggle FULL)
                   , ("M-S-f"       , sendMessage (T.Toggle "floats"))
                   , ("M-S-x"       , io (exitWith ExitSuccess))
                   , ("M-x"         , spawn $ "xmonad --recompile && xmonad --restart")
                   , ("M-<Esc>"     , spawn $ "xkill")

                   -- Layout shortcut
                   , ("M-S-l 0"     , sendMessage $ JumpToLayout "tall")
                   , ("M-S-l a"     , sendMessage $ JumpToLayout "accordion")
                   , ("M-S-l t"     , sendMessage $ JumpToLayout "tabs")
                   , ("M-S-l b"     , sendMessage $ JumpToLayout "fibonacci")

                   -- Emacs integration
                   , ("M-e"           , spawn myEditor)
                   , ("M-S-e"         , spawn $ emacsExec ++ "\"(emacs-everywhere)\"")

                   -- GridSelect
                   , ("M-g g"       , goToSelected $ myGridConfig myColorizer)
                   , ("M-g a"       , spawnSelected' myAppGrid)
                   , ("M-g s"       , spawnSelected' mySysGrid)
                   , ("M-g b"       , bringSelected $ myGridConfig myColorizer)

                   -- Dactyl Manuform 5x7 Hyper key
                   , ("M-M1-C-S-x g", goToSelected $ myGridConfig myColorizer)
                   , ("M-M1-C-S-x a", spawnSelected' myAppGrid)
                   , ("M-M1-C-S-x s", spawnSelected' mySysGrid)
                   , ("M-M1-C-S-x b", bringSelected $ myGridConfig myColorizer)

                   -- Named scratchpad
                   , ("M-s d"  , namedScratchpadAction myScratchpad  "dropdown")
                   , ("M-s h"  , namedScratchpadAction myScratchpad  "sys_monitor")

                   -- Office
                   , ("M-p"    , spawn $ "okular")
                   , ("M-s c", namedScratchpadAction myScratchpad  "calculator")
                   ]

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    ]

myLayout = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats
           $ mkToggle (NOBORDERS ?? FULL ?? EOT) myDefaultLayout
         where
           myDefaultLayout = tall
                             ||| tallAccordion
                             ||| spirals
                             ||| tabs
                             ||| floats

tall = renamed [Replace "tall"]
       $ smartBorders
       $ spacing myGaps
       $ Tall 1 (3/100) (1/2)

spirals = renamed [Replace "fibonacci"]
        $ smartBorders
        $ spacing myGaps
        $ spiral (6/7)

tabs = renamed [Replace "tabs"]
     $ tabbed shrinkText myTabTheme

tallAccordion = renamed [Replace "accordion"]
              $ Accordion

floats = renamed [Replace "floats"]
       $ smartBorders
       $ limitWindows 20 simplestFloat

myManageHook = composeAll . concat $
    [ [className =? "MPlayer"             --> doFloat]
    , [className =? "Gimp"                --> doFloat]
    , [className =? "guake"               --> doFloat]
    , [title     =? "update-arch"         --> doCenterFloat]
    , [title     =? "network_manager_tui" --> doCenterFloat]
    , [title     =? "Emacs Everywhere.*"    --> doCenterFloat]
    , [resource  =? "desktop_window"      --> doIgnore] ]

myLogHook = return ()

myStartupHook = do
    spawnOnce "~/.xmonad/scripts/autostart.sh"
    setWMName "LG3D"

myScratchpad =[ NS "dropdown"     spawnTerm              findTerm             manageTerm,
                NS "sys_monitor"  spawnHtop              findHtop             manageHtop,
                NS "calculator"   officeLaunchCalculator officeFindCalculator officeManageCalculator
              ]
        where
          spawnHtop              = myTerminal ++ " -t htop_term -e htop"
          findHtop               = title =? "htop_term"
          manageHtop             = doCenterFloat
          spawnTerm              = myTerminal ++ " -t dropdown -e tmux "
          findTerm               = title =? "dropdown"
          manageTerm             = doCenterFloat
          officeLaunchCalculator = "qalculate-gtk"
          officeFindCalculator   = title =? "Qalculate!"
          officeManageCalculator = doCenterFloat
--            where
--              h = 0.9
--              w = 0.9
--              t = 0.95 - h
--              l = 0.95 - w

myConfig = defaultConfig {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

        keys               = myKeys,
        mouseBindings      = myMouseBindings,

        layoutHook         = myLayout,
        manageHook         = myManageHook <+> manageDocks <+> namedScratchpadManageHook myScratchpad,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

main = do
  dbus <- D.connectSession
  D.requestName dbus (D.busName_ "org.xmonad.Log")
      [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  xmonad $ ewmh myConfig
    { handleEventHook = docksEventHook <+> fullscreenEventHook
    } `additionalKeysP` myAdditionalKeys
