-- This is my xmonad. There are many like it, but this one is mine.

import XMonad
import XMonad.Actions.Volume
import XMonad.Hooks.EwmhDesktops
    
import Data.Monoid
import System.Exit
import Graphics.X11.ExtraTypes.XF86
    
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    [ ((modm,               xK_q     ), spawn $ XMonad.terminal conf)
    --, ((modm,               xK_w     ), spawn "dmenu </dev/null | xargs -0 surf")
    , ((modm,               xK_w     ), spawn "firefox")
    , ((modm,               xK_e     ), spawn "emacsclient -c")
    , ((modm,               xK_z     ), spawn "slock")
    , ((modm,               xK_x     ), spawn "dmenu_run")
    , ((modm .|. shiftMask, xK_c     ), kill)

    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp  )
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp   )
      
    , ((modm,               xK_Return), windows W.focusMaster)
    , ((modm .|. shiftMask, xK_Return), windows W.swapMaster)

    , ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
      
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)
    , ((modm .|. shiftMask, xK_h     ), sendMessage (IncMasterN 1))
    , ((modm .|. shiftMask, xK_l     ), sendMessage (IncMasterN (-1)))
      
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    , ((modm,               xK_n     ), refresh)

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    , ((modm              , xK_p     ), spawn "xmonad --recompile; xmonad --restart")
    , ((modm .|. shiftMask, xK_p     ), io (exitWith ExitSuccess))
      
    ]

    ++

    [ ((0 , xF86XK_MonBrightnessDown ), spawn "xbacklight -10")
    , ((0 , xF86XK_MonBrightnessUp )  , spawn "xbacklight +10")
    , ((0 , xF86XK_AudioPrev )        , spawn "mpc prev")
    , ((0 , xF86XK_AudioPlay )        , spawn "mpc toggle")
    , ((0 , xF86XK_AudioNext )        , spawn "mpc next")
    , ((0 , xF86XK_AudioMute )        , toggleMute >> return () )
    , ((0 , xF86XK_AudioLowerVolume ) , lowerVolume 3 >> return () )
    , ((0 , xF86XK_AudioRaiseVolume ) , raiseVolume 3 >> return () )
    ]
    
    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

    ++

    -- alt-mod-{1,2,3}, Switch to physical/Xinerama screens 1, 2, or 3
    -- alt-mod-shift-{1,2,3}, Move client to screen 1, 2, or 3
    [((m .|. mod1Mask .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_1, xK_2, xK_3] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Window rules:

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
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------

main = xmonad $ ewmh $ def {
      -- simple stuff
        terminal           = "st"
      , focusFollowsMouse  = True
      , clickJustFocuses   = False
      , borderWidth        = 1
      , modMask            = mod4Mask
      , workspaces         = map show [1..9]
      , normalBorderColor  = "#dddddd"
      , focusedBorderColor = "#ff0000"

      -- key bindings
      , keys               = myKeys
      , mouseBindings      = myMouseBindings

      -- hooks, layouts
      , layoutHook         =
          let 
              tiled   = Tall nmaster delta ratio
              nmaster = 1
              ratio   = 1/2
              delta   = 3/100
          in tiled ||| Mirror tiled ||| Full
      , manageHook         = myManageHook
      , handleEventHook    = fullscreenEventHook
      , logHook            = return ()
      , startupHook        = return ()
    }
