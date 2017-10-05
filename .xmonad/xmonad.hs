{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
-- xmonad config used by Lisael
-- Author: Lisael
-- based on https://github.com/bitterjug/dotfiles/blob/master/xmonad/xmonad.hs
-- lots of good ideas from 

import Control.Monad  -- IDE
import System.IO
import System.Exit


import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.FloatKeys
import XMonad.Actions.Navigation2D
import XMonad.Actions.Navigation2D
import XMonad.Actions.Submap
import XMonad.Actions.TagWindows
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

import KeyDown

-- testing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation as Nav

import XMonad.Layout.Combo
import XMonad.Layout.TwoPane

-- local stuff (~/.xmonad/lib/)
import XMonad.Layout.IDE(IDE(..))
import XMonad.Actions.InputMode


myTerminal = "urxvt"

myLocker = "slock"

myScreenshot = "xfce4-screenshot -r"

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
    , className =? "Arandr"         --> doFloat
    , className =? "Steam"          --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Dialog"         --> doFloat
    , resource  =? "gpicview"       --> doFloat
    , className =? "MPlayer"        --> doFloat
    , className =? "Pinentry-gtk-2" --> doFloat
    , className =? "VirtualBox"     --> doShift "4:vm"
    , className =? "Xmessage"       --> doFloat
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

-- minMaxNamed :: ( LayoutClass l a , LayoutClass l' a ) => String -> l a -> l' a
minMaxNamed :: ( LayoutClass l Window ) => String -> l Window -> ModifiedLayout Rename ( ModifiedLayout Minimize ( ModifiedLayout Maximize l ) ) Window
minMaxNamed name layout =
    named name $ minimize (maximizeWithPadding 0 layout)

threeCol = minMaxNamed "=| |=" ( ThreeColMid 1 (3/100) (1/2) )
tall = minMaxNamed "[ ]=" ( Tall 1 (3/100) (1/2) )
full = minMaxNamed "[  ]" Full
ide = minMaxNamed "[|]=" ( IDE 1 (3/100) (2/3) )
grid = minMaxNamed "#" Grid 
large = minMaxNamed "ITTI" (Mirror $ Tall 3 (3/100) (4/5) )
combo = minMaxNamed "Combo" ( combineTwo (TwoPane 0.03 0.5) (Mirror $ Tall 3 (3/100) (4/5) ) (tabbed shrinkText def) )

realFull = noBorders (fullscreenFull Full)

nostruts = avoidStruts $ ide ||| large  ||| tall ||| grid ||| combo ||| threeCol ||| full





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

-- float InputMode
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

-- taggingMode :: XPConfig  -> [((KeyMask, KeySym), ( X () ))]
taggingMode =
    [
      ((0,           xK_t  ), tagPrompt def (\s -> withFocused (addTag s)))
    , ((controlMask, xK_t  ), tagDelPrompt def)
    , ((0,           xK_Tab), focusDown )
    ]

-- ide_tile :: X()
-- ide_tile= do
--     let wins=W.index (windows (\x -> x))
--     spawn "zenity --info --text='coucou'"
--
ide_tile' :: W.StackSet i l Window s sd -> W.StackSet i l Window s sd
ide_tile' = W.modify' $ \c -> c

ide_tile :: X()
ide_tile = do
    windows ide_tile'

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  ----------------------------------------------------------------------
  -- Custom key bindings
  --
    -- mic...
    [ ((modm,                 xK_Return), spawn $ XMonad.terminal conf) -- %! New terminal
    , ((modm .|. shiftMask,   xK_Return), windows W.swapMaster) -- %! Swap the focused window and the master window
    , ((modm,                 xK_x     ), spawn myLocker) -- %! Lock screen
    , ((modm,                 xK_p     ), spawn myLauncher) -- %! App launcher
    , ((modm .|. shiftMask,   xK_p     ), spawn "pass_finder.sh") -- %! Password finder
    , ((modm .|. controlMask, xK_p     ), spawn myScreenshot) -- %! Screenshot
    , ((modm,                 xK_b     ), sendMessage ToggleStruts) -- %! Hide status bar
    , ((modm .|. shiftMask,   xK_q     ), io (exitWith ExitSuccess)) -- %! Quit XMonad
    , ((modm,                 xK_q     ), restart "xmonad" True) -- %! Recompile and restart XMonad
    , ((modm,                 xK_c     ), spawn "CM_LAUNCHER=rofi clipmenu -dmenu -i -font 'mono 6' -p 'copy:' -scrollmethod 1 -lines 50 -hide-scollbar -line-margin 0") -- %! Open clipboard manager
    , ((0,               xF86XK_Display), spawn "arandr")
    -- , ((modm,                 xK_n), refresh)

    -- testing.
    , ((modm, xK_a), return ())
    -- , ((modm, xK_a), submap $ defaultSublMap conf)
    -- , ((modm .|. controlMask, xK_m), withFocused (sendMessage . MergeAll))
    -- , ((modm, xK_a), sendMessage $ Nav.Move Nav.R)
    , ((modm .|. controlMask .|. shiftMask, xK_Left ), sendMessage $ Nav.Move Nav.L)
    , ((modm .|. controlMask .|. shiftMask, xK_Up   ), sendMessage $ Nav.Move Nav.U)
    , ((modm .|. controlMask .|. shiftMask, xK_Down ), sendMessage $ Nav.Move Nav.D)
    -- , ((modm,                 xK_a     ), ide_tile)

    -- requires this in .xinitrc
    -- xmodmap -e "remove Lock = Caps_Lock"
    -- xmodmap -e "keycode  66 = XF86Mail NoSymbol XF86Mail"
    , ((0,                 xF86XK_Mail), return ())


    -- client control
    , ((modm .|. shiftMask,   xK_c    ), kill) -- %! Kill Client
    , ((modm .|. controlMask, xK_l    ), windowSwap R False) -- %! Swap window to the right
    , ((modm .|. controlMask, xK_h    ), windowSwap L False) -- %! Swap window to the left
    , ((modm .|. controlMask, xK_k    ), windowSwap U False) -- %! Swap window upwards
    , ((modm .|. controlMask, xK_j    ), windowSwap D False) -- %! Swap window downwards
    , ((modm,                 xK_n    ), withFocused minimizeWindow) -- %! Minimize window
    , ((modm .|. shiftMask,   xK_n    ), sendMessage RestoreNextMinimizedWin) -- %! Restore minimixed window
    -- Sticky windows
    , ((modm,                 xK_s    ), windows copyToAll) -- %! Sticky window
    , ((modm .|. shiftMask,   xK_s    ), killAllOtherCopies) -- %! Not sticky
    , ((modm,                 xF86XK_Mail), withFocused (sendMessage . maximizeRestore)) -- %! Toggle Fullscreen window

    , ((modm,                 xK_y    ), inputMode taggingMode) -- %! Toggle tagging mode
    -- , ((modm,                 xK_y  ), tagPrompt def (\s -> withFocused (addTag s)))
    -- , ((modm .|. controlMask, xK_y  ), tagDelPrompt def)

    -- floating: move and resize with h, j, k, l. These are modes. Just hit modm+g and then
    -- use plain h,j,k,l. Hit Esc when done
    , ((modm,                 xK_g    ), inputMode floatCtlMode) -- %! Toggle floatCtl

    -- workspaces
    , ((modm,                 xK_Escape    ), toggleWS) -- %! Toggle last workspace
    , ((modm,                 xK_Right     ), nextWS) -- %! Next Workspace
    , ((modm,                 xK_Left      ), prevWS) -- %! Previous Workspace
    , ((modm,                 xK_v         ), do markBoring -- %! Mark window as boring
                                                 focusDown)
    , ((modm .|. shiftMask,   xK_v        ), clearBoring) -- %! Mark all windows as not boring
    , ((modm .|. controlMask, xK_v        ), spawn "/borewin.sh") -- %! Iteractive boring
    , ((modm .|. shiftMask,   xK_BackSpace), removeWorkspace) -- %! Delete current workspace
    , ((modm .|. shiftMask,   xK_r        ), renameWorkspace def) -- %! Rename current workspace

    -- projects
    , ((modm, xK_semicolon), switchProjectPrompt defaultXPConfig ) -- %! Toggle Projects prompt

    -- layout
    , ((modm,                 xK_space ), sendMessage NextLayout) -- %! Cycle layouts
    , ((modm .|. shiftMask,   xK_space ), setLayout $ XMonad.layoutHook conf) -- %! Default layout
    , ((modm .|. shiftMask,   xK_l     ), windowGo R False) -- %! Move to the window on the right
    , ((modm .|. shiftMask,   xK_h     ), windowGo L False) -- %! Move to the window on the right
    , ((modm .|. shiftMask,   xK_k     ), windowGo U False) -- %! Move to the window on the right
    , ((modm .|. shiftMask,   xK_j     ), windowGo D False) -- %! Move to the window on the right
    , ((modm,                 xK_Tab   ), focusDown) -- %! Next not boring Window
    , ((modm,                 xK_grave ), dwmpromote) -- %! Promote Window as master or switch master
    , ((modm,                 xK_j     ), windows W.focusDown) -- %! Next window (incl. boring)
    , ((modm,                 xK_k     ), windows W.focusUp  ) -- %! Prev window (incl. boring)
    -- , ((modm,                 xK_m     ), windows W.focusMaster  ) -- %! Move focus to master
    , ((modm,                 xK_h     ), sendMessage Shrink) -- %! Shrink master area
    , ((modm,                 xK_l     ), sendMessage Expand) -- %! Expand master area
    , ((modm,                 xK_t     ), withFocused $ windows . W.sink) -- %! Push floating window to tiling
    , ((modm,                 xK_comma ), sendMessage (IncMasterN 1)) -- %! Increment number of master windows
    , ((modm,                 xK_period), sendMessage (IncMasterN (-1))) -- %! Decrement the number of Master windows

    -- media control
    , ((0, xF86XK_AudioMute           ), spawn "amixer -q set Master toggle")
    , ((0, xF86XK_AudioLowerVolume    ), spawn "amixer -q set Master 5%-")
    , ((0, xF86XK_AudioRaiseVolume    ), spawn "amixer -q set Master 5%+")

    ]

    -- mod-[1..9]       %! Switch to workspace N in the list of workspaces
    -- mod-shift-[1..9] %! Move client to workspace N in the list of workspaces
    ++
    zip (zip (repeat (modm)) ([xK_1..xK_9] ++ [xK_0])) (map (withNthWorkspace W.greedyView) [0..])
    ++
    zip (zip (repeat (modm .|. shiftMask)) ( [xK_1..xK_9] ++ [xK_0] )) (map (withNthWorkspace W.shift) [0..])

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


statusShowWSIndex :: WorkspaceId -> String
statusShowWSIndex wsId =
    wsId

------------------------------------------------------------------------
-- myStartupHook = return ()
myLogHook xmproc = do
    dynamicLogWithPP $ xmobarPP {
            ppOutput = hPutStrLn xmproc
          , ppHidden = statusShowWSIndex
          , ppTitle = xmobarColor xmobarTitleColor "" . shorten 100
          , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor "" . wrap "[" "]"
          , ppSep = " | "
    }
    updatePointer (0.5, 0.5) (0, 0)
    workspaceHistoryHook
    logHook desktopConfig

myHandleEventHook = minimizeEventHook <+> keyDownEventHook

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
    handleEventHook    = myHandleEventHook <+> handleEventHook desktopConfig
}

