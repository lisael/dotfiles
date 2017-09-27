{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
-- xmonad config used by Lisael
-- Author: Lisael
-- based on https://github.com/bitterjug/dotfiles/blob/master/xmonad/xmonad.hs
-- lots of good ideas from 

import System.IO
import System.Exit
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.FloatKeys
import XMonad.Actions.Navigation2D
import XMonad.Actions.Navigation2D
import XMonad.Actions.Submap
import XMonad.Actions.UpdatePointer
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Minimize
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory
import XMonad.Layout.BoringWindows
import XMonad.Layout.Fullscreen
import XMonad.Layout.Grid
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.LayoutModifier(ModifiedLayout)
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed(Rename)
import XMonad.Layout.Spiral
import XMonad.Layout.Stoppable
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Prompt
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W
import qualified Data.Map        as M


-- local stuff (~/.xmonad/lib/)

import XMonad.Layout.IDE(IDE(..))
import XMonad.Actions.InputMode


------------------------------------------------------------------------
-- Terminal
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal = "urxvt"

-- The command to lock the screen or show the screensaver.
myScreensaver = "slock"

-- The command to take a selective screenshot, where you select
-- what you'd like to capture on the screen.
mySelectScreenshot = "select-screenshot"

-- The command to take a fullscreen screenshot.
myScreenshot = "screenshot"

-- The command to use as a launcher, to launch commands that don't have
-- preset keybindings.
myLauncher = "$(xboomx)"

myEditor="urxvt -e zsh -c kak"

myIDEEditor="urxvt -e zsh -c 'kak -e \"ide-init\"'"


myProjects :: [Project]
myProjects =
  [ Project { projectName      = "peopleask"
            , projectDirectory = "~/projects/novapost/peopleask/"
            , projectStartHook = Just $ do spawn "git fetch"
                                           spawn "urxvt"
                                           spawn ("sleep 0.2 && " ++ myIDEEditor)
            }
  , Project { projectName      = "rrs"
            , projectDirectory = "~/projects/novapost/railroad-switch/"
            , projectStartHook = Just $ do spawn "git fetch"
                                           spawn "urxvt"
                                           spawn ("sleep 0.2 && " ++ myIDEEditor)
            }
  , Project { projectName      = "xmonad"
            , projectDirectory = "~/.xmonad"
            , projectStartHook = Just $ do spawn "urxvt"
                                           spawn ("sleep 0.2 && " ++ myIDEEditor)
            }
  , Project { projectName      = "kakide"
            , projectDirectory = "~/.config/kak"
            , projectStartHook = Just $ do spawn "urxvt"
                                           spawn ("sleep 0.2 && " ++ myIDEEditor)
            }
  ]



------------------------------------------------------------------------
-- Workspaces
-- The default number of workspaces (virtual screens) and their names.
--
myWorkspaces = ["1:www"] ++ map show [2..5]


------------------------------------------------------------------------
-- Window rules
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "Chromium"       --> doShift "1:www"
    , className =? "Firefox"        --> doShift "1:www"
    , resource  =? "desktop_window" --> doIgnore
    , className =? "Galculator"     --> doFloat
    , className =? "Steam"          --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "gpicview"       --> doFloat
    , className =? "MPlayer"        --> doFloat
    , className =? "VirtualBox"     --> doShift "4:vm"
    , className =? "Xchat"          --> doShift "5:media"
    , className =? "stalonetray"    --> doIgnore
    , isFullscreen --> (doF W.focusDown <+> doFullFloat)]


------------------------------------------------------------------------
-- Layouts
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--

-- TODO: rewrite these in a more haskell-ish way

-- minMaxNamed :: ( LayoutClass l Window , LayoutClass l' Window ) => String -> l Window -> l' Window
minMaxNamed :: ( LayoutClass l Window ) => String -> l Window -> ModifiedLayout Rename ( ModifiedLayout Minimize ( ModifiedLayout Maximize l ) ) Window
minMaxNamed name layout =
    named name $ minimize (maximizeWithPadding 0 layout)

threeCol = minMaxNamed "3Col2" ( ThreeColMid 1 (3/100) (1/2) )
tall = minMaxNamed "Tall" ( Tall 1 (3/100) (1/2) )
full = minMaxNamed "Full" Full 
ide = minMaxNamed "IDE" ( IDE 1 (3/100) (2/3) )
grid = minMaxNamed "#" Grid 
large= minMaxNamed "LIDE" ( Mirror ( Tall 3 (3/100) (4/5) ) )

realFull = noBorders (fullscreenFull Full)

nostruts = avoidStruts ( ide ||| large  ||| tall ||| grid ||| threeCol ||| full )

myLayout = boringWindows ( nostruts ||| realFull )

------------------------------------------------------------------------
-- Colors and borders
-- Currently based on the ir_black theme.
--
myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#ffb6b0"

-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabConfig = defaultTheme {
    activeBorderColor = "#7C7C7C",
    activeTextColor = "#CEFFAC",
    activeColor = "#000000",
    inactiveBorderColor = "#7C7C7C",
    inactiveTextColor = "#EEEEEE",
    inactiveColor = "#000000"
}

-- Color of current window title in xmobar.
xmobarTitleColor = "#FFB6B0"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#CEFFAC"

-- Width of the window border in pixels.
myBorderWidth = 1


------------------------------------------------------------------------
-- Key bindings
--
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod4Mask

floatCtlMode =
    [
    -- move fast
      ((0, xK_h), withFocused (keysMoveWindow (-15,0) ) )
    , ((0, xK_l), withFocused (keysMoveWindow (15,0) ) )
    , ((0, xK_k), withFocused (keysMoveWindow (0,-15) ) )
    , ((0, xK_j), withFocused (keysMoveWindow (0,15) ) )
    -- move precisely
    , ((controlMask, xK_h), withFocused (keysMoveWindow (-1,0) ) )
    , ((controlMask, xK_l), withFocused (keysMoveWindow (1,0) ) )
    , ((controlMask, xK_k), withFocused (keysMoveWindow (0,-1) ) )
    , ((controlMask, xK_j), withFocused (keysMoveWindow (0,1) ) )
    -- resize fast
    , ((shiftMask, xK_h), withFocused (keysResizeWindow (-15,0) (0,0)))
    , ((shiftMask, xK_l), withFocused (keysResizeWindow (15,0) (0,0)))
    , ((shiftMask, xK_k), withFocused (keysResizeWindow (0,-15) (0,0)))
    , ((shiftMask, xK_j), withFocused (keysResizeWindow (0,15) (0,0)))
    -- resize precisely
    , ((shiftMask .|. controlMask, xK_h), withFocused (keysResizeWindow (-1,0) (0,0)))
    , ((shiftMask .|. controlMask, xK_l), withFocused (keysResizeWindow (1,0) (0,1)))
    , ((shiftMask .|. controlMask, xK_k), withFocused (keysResizeWindow (0,-1) (0,0)))
    , ((shiftMask .|. controlMask, xK_j), withFocused (keysResizeWindow (0,1) (1,0)))
    -- cycle windows
    , ((0,         xK_Tab), focusDown )
    ]


myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  ----------------------------------------------------------------------
  -- Custom key bindings
  --
    -- mic...
    [ ((modm,                 xK_Return), spawn $ XMonad.terminal conf)
    , ((modm .|. shiftMask,   xK_Return), windows W.swapMaster) -- %! Swap the focused window and the master window
    , ((modm,                 xK_x     ), spawn myScreensaver)
    , ((modm,                 xK_p     ), spawn myLauncher)
    , ((modm,                 xK_b     ), sendMessage ToggleStruts) -- hide status bar
    , ((modm .|. shiftMask,   xK_q), io (exitWith ExitSuccess))
    , ((modm,                 xK_q), restart "xmonad" True)
    , ((modm,                 xK_c), spawn "CM_LAUNCHER=rofi clipmenu -dmenu -i -font 'mono 6' -p 'copy:' -scrollmethod 1 -lines 50 -hide-scollbar -line-margin 0")
    , ((modm,                 xK_x), withFocused (sendMessage . maximizeRestore))

    -- requires this in .xinitrc
    -- xmodmap -e "remove Lock = Caps_Lock"
    -- xmodmap -e "keycode  66 = XF86Mail NoSymbol XF86Mail"
    , ((0,                 xF86XK_Mail), spawn "/home/lisael/bin/easymove.sh")

    -- screeshot
    , ((modm .|. shiftMask,   xK_p     ), spawn mySelectScreenshot)
    , ((modm .|. controlMask .|. shiftMask, xK_p), spawn myScreenshot)

    -- media control
    , ((0, xF86XK_AudioMute           ), spawn "amixer -q set Master toggle")
    , ((0, xF86XK_AudioLowerVolume    ), spawn "amixer -q set Master 5%-")
    , ((0, xF86XK_AudioRaiseVolume    ), spawn "amixer -q set Master 5%+")

    -- client control
    , ((modm .|. shiftMask,   xK_c    ), kill)
    , ((modm .|. controlMask, xK_l    ), windowSwap R False)
    , ((modm .|. controlMask, xK_h    ), windowSwap L False)
    , ((modm .|. controlMask, xK_k    ), windowSwap U False)
    , ((modm .|. controlMask, xK_j    ), windowSwap D False)
    , ((modm,                 xK_n    ), withFocused minimizeWindow)
    , ((modm .|. shiftMask,   xK_n    ), sendMessage RestoreNextMinimizedWin)
    , ((modm,                 xK_v    ), windows copyToAll) -- @@ Make focused window always visible
    , ((modm .|. shiftMask,   xK_v    ), killAllOtherCopies) -- @@ Toggle window state back

    -- floating: move and resize with h, j, k, l. These are modes. Just hit modm+g and then
    -- use plain h,j,k,l. Hit Esc when done
    , ((modm,                 xK_g), inputMode floatCtlMode)

    -- workspaces
    , ((modm,                 xK_Escape ), toggleWS)
    , ((modm,                 xK_Right  ), nextWS)
    , ((modm,                 xK_Left   ), prevWS)
    , ((modm,                 xK_b      ), do markBoring
                                              focusDown)
    , ((modm .|. shiftMask,   xK_b      ), clearBoring)
    , ((modm .|. controlMask, xK_b      ), spawn "/home/lisael/bin/borewin.sh")
    , ((modm .|. shiftMask, xK_BackSpace), removeWorkspace)
    , ((modm .|. shiftMask, xK_r      ), renameWorkspace def)

    -- projects
    , ((modm, xK_semicolon), switchProjectPrompt defaultXPConfig )

    -- layout
    , ((modm,                 xK_space), sendMessage NextLayout)
    , ((modm .|. shiftMask,   xK_space), setLayout $ XMonad.layoutHook conf)
    , ((modm .|. shiftMask,   xK_l    ), windowGo R False)
    , ((modm .|. shiftMask,   xK_h    ), windowGo L False)
    , ((modm .|. shiftMask,   xK_k    ), windowGo U False)
    , ((modm .|. shiftMask,   xK_j    ), windowGo D False)
    , ((modm,                 xK_Tab), focusDown)

    -- , ((modm,                 xK_n), refresh)
    , ((modm,                 xK_j), windows W.focusDown)
    , ((modm,                 xK_k), windows W.focusUp  )
    , ((modm,                 xK_m), windows W.focusMaster  )
    , ((modm,                 xK_h), sendMessage Shrink)
    , ((modm,                 xK_l), sendMessage Expand)
    , ((modm,                 xK_t), withFocused $ windows . W.sink)
    , ((modm,                 xK_comma), sendMessage (IncMasterN 1))
    , ((modm,                 xK_period), sendMessage (IncMasterN (-1)))
    , ((modm,                 xK_6), withNthWorkspace W.greedyView 5)
    ]

    -- mod-[1..9]       %! Switch to workspace N in the list of workspaces
    -- mod-shift-[1..9] %! Move client to workspace N in the list of workspaces
    ++
    zip (zip (repeat (modm)) [xK_1..xK_9]) (map (withNthWorkspace W.greedyView) [0..])
    ++
    zip (zip (repeat (modm .|. shiftMask)) [xK_1..xK_9]) (map (withNthWorkspace W.shift) [0..])

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    -- [((m .|. modm, k), windows $ f i)
        -- | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_5]
        -- , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
        -- ++

    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    ++
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings
--
-- Focus rules
-- True if your focus should follow your mouse cursor.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]


------------------------------------------------------------------------
-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--


-- myStartupHook = return ()

myLogHook xmproc = do
    dynamicLogWithPP $ xmobarPP {
            ppOutput = hPutStrLn xmproc
          , ppTitle = xmobarColor xmobarTitleColor "" . shorten 100
          , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
          , ppSep = "   "
    }
    updatePointer (0.5, 0.5) (0, 0)
    workspaceHistoryHook
    logHook desktopConfig

myHandleEventHook = minimizeEventHook

------------------------------------------------------------------------
-- Run xmonad with all the defaults we set up.
--
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad 
    $ ewmh
    $ dynamicProjects myProjects
    $ myConfig xmproc


------------------------------------------------------------------------
-- Combine it all together
myConfig xmproc = desktopConfig {
    -- simple stuff
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,

    -- key bindings
    keys               = myKeys,
    mouseBindings      = myMouseBindings,

    -- hooks, layouts
    layoutHook         = smartBorders $ myLayout,
    manageHook         = manageDocks <+> myManageHook <+> manageHook desktopConfig,
    -- startupHook        = myStartupHook,
    logHook            = myLogHook xmproc,
    handleEventHook  = myHandleEventHook <+> handleEventHook desktopConfig
}

