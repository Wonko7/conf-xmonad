{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses, FlexibleContexts #-}

import Data.Bits
import Data.List
import Control.Monad (liftM, filterM, join)

-- Import stuff
import XMonad
import XMonad.Config.Desktop
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified Data.Set as DS
import XMonad.Util.EZConfig--(additionalKeys)
import System.Exit
import Graphics.X11.Xlib
import System.IO
import Data.Ratio ((%))
import Data.IORef

import XMonad.Actions.DynamicWorkspaces
import XMonad.Hooks.FadeInactive

-- actions
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleWindows
import XMonad.Actions.WindowGo
import XMonad.Actions.Navigation2D
import qualified XMonad.Actions.Search as S
import qualified XMonad.Actions.Submap as SM
import XMonad.Actions.GridSelect
import XMonad.Actions.SpawnOn
import XMonad.Core as XMonad hiding
        (focusFollowsMouse)
import XMonad.Actions.UpdatePointer
import XMonad.Actions.TopicSpace
import XMonad.Actions.WithAll
import XMonad.Actions.GroupNavigation
import XMonad.Actions.RotSlaves

-- utils
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run(spawnPipe)
import qualified XMonad.Prompt as P
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Window

-- hooks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers

-- layouts
import XMonad.Layout.SimplestFloat
import XMonad.Layout.NoBorders
import XMonad.Layout.Accordion
import XMonad.Layout.Reflect
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Grid
import XMonad.Layout.Spacing
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.LayoutHints
import XMonad.Layout.MagicFocus
import XMonad.Layout.IM
import XMonad.Layout.LayoutModifier
import XMonad.Layout.ZoomRow
import XMonad.Util.WindowProperties

-- local stuff:
import XMonad.Layout.EqualSpacing
import XMonad.MyStuff.AddRosters

main = do
  toggleFadeSet <- newIORef DS.empty
  xmonad $ ewmh desktopConfig
    {   keys              = myKeys toggleFadeSet
      , mouseBindings     = myMouseBindings
      , terminal          = myTerminal
      , workspaces        = myTopics
      , layoutHook        = myLayouts
      , logHook           = myLogHook toggleFadeSet
      , focusFollowsMouse = myFocusFollowsMouse
      , borderWidth       = myBorderWidth
      , manageHook        = composeAll
      [ className =? "Tor Browser" --> doCenterFloat -- for security reasons! window size is fingerprintable!
        , className =? "lxqt-panel" --> doIgnore
        , isDialog --> unfloat
      ]
      <+> composeAll
      [ manageDocks ]
    }
      where unfloat = ask >>= doF . W.sink

myLayouts = noBorders . mkToggle (NOBORDERS ?? FULL ?? EOT) $ avoidStruts $ equalSpacing 30 0 0 5 $
    onWorkspaces ["1"] mediaLayouts $
    onWorkspaces ["2"] imTooSquare $
    onWorkspaces ["3"] weAllFloatDownHere $
    onWorkspaces ["6", "7", "8"] workLayouts $
    onWorkspaces ["4", "5"] browsersLayouts $
    onWorkspaces ["9"] imLayouts $
    onWorkspaces ["10"] workLayouts $
    onWorkspaces ["11"] browsersLayouts
    defLayouts
  where
     workLayouts        = magicFocus (Mirror wtiled) ||| magicFocus wtiled ||| Mirror tiled ||| tiled
     defLayouts         = tiled ||| magicFocus (Mirror wtiled) ||| magicFocus wtiled ||| Mirror tiled
     imLayouts          = reflectHoriz $ withIMs (1/6) rosters $ Tall 0 delta ratio
     rosters            = [pidginRoster]
     pidginRoster       = And (ClassName "Pidgin") (Role "buddy_list")
     telRoster          = And (ClassName "Ktp-contactlist") (Role "MainWindow")
     weAllFloatDownHere = simplestFloat ||| Accordion
     imTooSquare        = Grid ||| Mirror zoomRow
     browsersLayouts    = Mirror Accordion ||| magicFocus wtiled ||| Accordion ||| tiled ||| magicFocus (Mirror wtiled) ||| Mirror tiled -- not that I ever use anything other than mirror accor...
     mediaLayouts       = magicFocus (Mirror wtiled) ||| magicFocus wtiled
     -- default tiling algorithm partitions the screen into two panes
     tiled              = layoutHints $ Tall nmaster delta ratio
     wtiled             = layoutHints $ Tall nmaster delta (4/5)
     imtiled            = layoutHints $ Tall 2 delta (4/5)
     -- The default number of windows in the master pane
     nmaster            = 1
     -- Default proportion of screen occupied by master pane
     ratio              = 2/3
     -- Percent of screen to increment by when resizing panes
     delta              = 3/100

myBorderWidth = 0

myTerminal :: String
myTerminal = "~/conf/misc/scripts/st.sh"
myBrowser  = "firefox"

spawnTmuxSession name               = spawn $ "LOAD_TMUX_SESSION=" ++ name ++ " " ++ myTerminal
spawnRemoteTmuxSession host session = spawn $ myTerminal ++ " -e ssh -t " ++ host ++ " LOAD_TMUX_SESSION=" ++ session ++ " zsh"
spawnCmd cmd                        = spawn $ myTerminal ++ " -e " ++ cmd

myTopics :: [Topic]
myTopics = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"]

myTopConf :: TopicConfig
myTopConf = def
  {   topicDirs = M.fromList [(show i, "~/") | i <- [1..9]]
    , defaultTopic = "1"
    , defaultTopicAction = const $ return ()
    , topicActions = M.fromList
      [   ("1", spawnTmuxSession "gentoo")
        , ("2",    spawnRemoteTmuxSession "wg.nostromo.local" "gentoo"
                >> spawnRemoteTmuxSession "wg.undefined.local" "gentoo"
                >> spawnRemoteTmuxSession "5.39.77.155" "gentoo"
                -- >> spawnCmd "ssh -t root@5.39.77.155 tmux attach -t install"
                -- >> spawnRemoteTmuxSession "wg.serenity.local" "gentoo"
                -- >> spawnRemoteTmuxSession "wg.daban-urnud.local" "gentoo"
          )
  -- [((modm .|. shiftMask, k), windows $ W.shift $ show i) | (i, k) <- zip [1..9] [xK_1..xK_9]]
        , ("3", spawnHere "~/local/tor-browser_en-US/Browser/start-tor-browser")
        , ("4", spawnHere "firefox -P uman")
        , ("8", spawnTmuxSession "2m")
        , ("9", spawnTmuxSession "chat" >> spawnHere "pidgin")
        , ("10", spawnHere $ myTerminal ++ " -e tmux")
        , ("11", spawn "VIM_SERVER=DANCE_COMMANDER ~/conf/vim/nvim.sh")
        , ("12", spawnHere "firefox -P small")
      ]
  }

addTopicHist = do winset <- gets windowset;
                  setLastFocusedTopic (W.currentTag winset) (const True)

toggleTopics = do addTopicHist;
                  switchNthLastFocused myTopConf 1;

goToTopic i = do addTopicHist;
                 switchTopic myTopConf i;

prevTS = do addTopicHist;
            prevWS;

nextTS = do addTopicHist;
            nextWS;


myLogHook :: IORef (DS.Set Window) -> X ()
myLogHook toggleFadeSet = historyHook >> fadeOutLogHook (fadeIf (fadeCondition toggleFadeSet) 0.8)

fadeCondition :: IORef (DS.Set Window) -> Query Bool
fadeCondition floats =
  (isUnfocused <||> (className =? "Conky"))
  <&&> (join . asks $ \w -> liftX . io $ DS.notMember w `fmap` readIORef floats)

toggleFadeOut :: Window -> DS.Set Window -> DS.Set Window
toggleFadeOut w s | w `DS.member` s = DS.delete w s
                  | otherwise = DS.insert w s

-- >  -- in keybindings: there are much more W.swap* W.focus* and W.shift* functions to use.
-- >  ,((modm, xK_f), onAllWS W.focusDown)
-- >  ,((modm, xK_d), onAllWS W.focusUp)
-- >  ,((modm, xK_g), onAllWS W.swapMaster)
-- doNotFadeOutWindows = title ~? "Call with " <||> className =? "xine" <||> className =? "MPlayer" ===> FIXME title could be intersting!

ks toggleFadeSet conf@XConfig {XMonad.modMask = modm} = [
    -- terminal
    -- fixme: nothing on D! xk_d, B neither. b & d & g v & 0 & shift v & shift 0 & shift t, shift z, shift period for something fun.
    ((modm, xK_Return),                     spawnHere $ myTerminal ++ " -e tmux")
  , ((modm .|. shiftMask,   xK_Return),     spawnHere myTerminal)
    -- close focused window
  , ((modm .|. shiftMask,   xK_c),          kill)
    ---------
      -- Rotate through the available layout algorithms
  , ((modm,                 xK_Tab),        sendMessage NextLayout)
    -- Reset the layouts on the current workspace to default
  , ((modm .|. shiftMask,   xK_Tab),        setLayout $ XMonad.layoutHook conf)
    ---------
      -- Shrink/expand the master area
  , ((modm,                 xK_u),          sendMessage Shrink)
  , ((modm,                 xK_i),          sendMessage Expand)
    ---------
      -- send window to next WS
  , ((modm .|. shiftMask,   xK_h),          shiftToPrev >> prevTS)
  , ((modm .|. shiftMask,   xK_l),          shiftToNext >> nextTS)
    -- next WS
  , ((modm,                 xK_h),          prevTS)
  , ((modm,                 xK_l),          nextTS)
    ---------
  , ((modm,                 xK_space),      toggleTopics)
  , ((modm .|. shiftMask,   xK_space),      nextMatch History (return True)) -- FIXME useless
    --, ((modm , xK_BackSpace), cycleRecentWindows [xK_Alt_L] xK_j xK_k)
    --, ((modm , xK_BackSpace), goToSelected gridselectWindow)
    --, ((modm , xK_BackSpace), nextMatch History (return True))
  , ((modm,                 xK_y),          currentTopicAction myTopConf)
    -- Move focus to the next window
  , ((modm,                 xK_j),          windows W.focusDown)
    -- Move focus to the previous window
  , ((modm,                 xK_k),          windows W.focusUp)
    -- Swap the focused window with the next window
  , ((modm .|. shiftMask,   xK_j),          windows W.swapDown)
    -- Swap the focused window with the previous window
  , ((modm .|. shiftMask,   xK_k),          windows W.swapUp)
    --------- 2d nav:
  , ((modm .|. controlMask, xK_j),          windowGo D False)
  , ((modm .|. controlMask, xK_k),          windowGo U False)
  , ((modm .|. controlMask, xK_h),          windowGo L False)
  , ((modm .|. controlMask, xK_l),          windowGo R False)
    -- Move focus to the master window
  , ((modm,                 xK_m),          windows W.swapMaster)
    -- ((modm .|. shiftMask, xK_m), sendMessage (Toggle SMARTBORDERS))
  , ((modm,                 xK_f),          sendMessage (Toggle FULL)) -- ; todo:change: toggle max
    -- Increment the number of windows in the master area
  , ((modm,                 xK_comma),      sendMessage (IncMasterN 1))
  , ((modm,                 xK_period),     sendMessage (IncMasterN (-1)))
    -- pause and resume duns notifs
  , ((modm,                 xK_quoteright), spawn "killall -SIGUSR1 dunst")
  , ((modm .|. shiftMask,   xK_quoteright), spawn "killall -SIGUSR2 dunst")
    -- Toggle the status bar gap -- Use this binding with avoidStruts from Hooks.ManageDocks.
  , ((modm .|. shiftMask,   xK_b),          sendMessage ToggleStruts)
    --  , ((modm .|. shiftMask,   xK_b),          SM.submap . M.fromList $ FIXME UP FOR GRABS
  , ((modm, xK_a),                          SM.submap . M.fromList $
    [   ((0, xK_c), spawnHere "calibre")
      , ((0, xK_p), spawnHere "pavucontrol-qt")
      , ((0, xK_w), spawnHere "wireshark")
      , ((0, xK_t), spawnHere "transmission-qt")
      , ((0, xK_s), SM.submap . M.fromList $
        [   ((0, xK_c), spawnTmuxSession "clj")
          , ((0, xK_u), spawnTmuxSession "2m")
          , ((0, xK_w), spawnTmuxSession "2m")
          , ((0, xK_g), spawnTmuxSession "gentoo")
        ])
      , ((0, xK_b), SM.submap . M.fromList $
        [   ((0, xK_q), spawnHere "qutebrowser")
          , ((0,         xK_c), spawnHere "chromium")
          , ((0,         xK_f), spawnHere "firefox -P uman")
          , ((shiftMask, xK_f), spawnHere "firefox --ProfileManager --new-instance")
          , ((0,         xK_o), spawnHere "opera")
          , ((0,         xK_t), spawnHere "~/local/tor-browser_en-US/start-tor-browser")
        ])
    ])
    -- Push window back into tiling
  , ((modm,               xK_t),       sinkAll)
    --------- reset mouse pointer
    --, ((modm , xK_z), updatePointer $ Relative 0.5 0.5) -- nope, never ever use this
    -- Quit xmonad
    -- , ((modm .|. shiftMask , xK_q), io (exitWith ExitSuccess))
  --, ((modm .|. shiftMask, xK_q),       spawn "qdbus org.kde.ksmserver /KSMServer logout -1 -1 -1")
    -- Restart xmonad
  , ((modm,               xK_q),       spawn "xmonad --recompile; xmonad --restart")
    -- group nav: useless shite.
  , ((modm,               xK_0),       goToTopic $ show 11)
  , ((modm .|. shiftMask, xK_0),       windows $ W.shift $ show 11)
  , ((modm,               xK_v),       nextMatchOrDo Forward (className =? "Gvim") (spawnHere "~/local/bin/gvim"))
  , ((modm .|. shiftMask, xK_v),       nextMatchOrDo Backward (className =? "Gvim") (spawnHere "~/local/bin/gvim"))
    -- launch stuff!
  --, ((modm,               xK_z),       spawn "xscreensaver-command --lock")
  , ((modm .|. shiftMask, xK_i),       spawnHere "urxvt")
  -- , ((modm,               xK_z),       spawn "xscreensaver-command --lock")
  --
  , ((modm, xK_z),                          SM.submap . M.fromList $
    [   ((modm,      xK_z),      spawn "xscreensaver-command --lock")
      , ((0,         xK_f),      withFocused $ io . modifyIORef toggleFadeSet . toggleFadeOut)
      , ((0,         xK_7),      goToTopic $ show 10)
      , ((0,         xK_8),      goToTopic $ show 11)
      , ((0,         xK_4),      goToTopic $ show 12)
      , ((0,         xK_1),      goToTopic $ show 10)
      , ((0,         xK_2),      goToTopic $ show 11)
      , ((0,         xK_3),      goToTopic $ show 12)
    ])
    -- , ((modm .|. shiftMask, xK_period),  spawnTmuxSession "clj") -- not sure
    -- FIXME do something better with this!
    --, ((modm .|. shiftMask, xK_t),       spawnHere "transmission-qt") -- FIXME up for grabs
    -- hotkeys:
    -- -- XF86AudioMute
  , ((0,                  0x1008ff12), spawn "pactl set-sink-mute 0 toggle; pactl set-sink-mute 1 toggle")
    -- "XF86AudioRaiseVolume"
  , ((0,                  0x1008ff13), spawn "pactl set-sink-volume 0 +10%; pactl set-sink-volume 1 +10%")
    -- XF86AudioLowerVolume
  , ((0,                  0x1008ff11), spawn "pactl set-sink-volume 0 -10%; pactl set-sink-volume 1 -10%")
    -- XF86AudioLowerVolume -- toggle mic
  , ((0,                  0x1008ffb2), spawn "pactl set-source-mute 0 toggle; pactl set-source-mute 1 toggle")
    -- brightness
  , ((0,                  0x1008ff02), spawn "light -A 10")
  , ((0,                  0x1008ff03), spawn "light -U 10")
    -- should be kill wifi, suspend for me:
    --, ((0, 0x1008ff95), spawn "systemctl suspend")
    -- 0x1008ff81, should be XF86Tools, hybrid sleep:
  , ((modm .|. shiftMask, xK_F10),     spawn "systemctl hibernate")
    -- XF86Search suspend:
  , ((modm .|. shiftMask, xK_F11),     spawn "systemctl hybrid-sleep")
    -- 0x1008ff4a, XF86LaunchA
    -- 0x1008ff4a
  , ((modm .|. shiftMask, xK_F12),     spawn "systemctl suspend")
  --
  --, (( modm, xK_b ), screenWorkspace 0 >>= flip whenJust (windows . W.view))
  --, (( modm, xK_d ), screenWorkspace 1 >>= flip whenJust (windows . W.view))
  , ((modm, xK_b), nextScreen)
  ]
  ++
  [((modm, k), goToTopic $ show i) | (i, k) <- zip [1..9] [xK_1..xK_9]]
  ++
  [((modm .|. shiftMask, k), windows $ W.shift $ show i) | (i, k) <- zip [1..9] [xK_1..xK_9]]
--  ++
--  [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
--    | (key, sc) <- zip [xK_g, xK_v, xK_d] [0..]
--    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]



modalmode toggleFadeSet conf@XConfig {XMonad.modMask = modm} = [ ((m `xor` modm, k), a >> (SM.submap . M.fromList $ modalmode toggleFadeSet conf)) | ((m, k), a) <- ks toggleFadeSet conf ]


myKeys :: IORef (DS.Set Window) -> XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys toggleFadeSet conf@XConfig {XMonad.modMask = modm} = M.fromList $ ((modm, xK_n), SM.submap . M.fromList $ modalmode toggleFadeSet conf) : ks toggleFadeSet conf

myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList
  [     ((modm, button1), \w -> focus w >> mouseMoveWindow w>> windows W.shiftMaster)
      -- mod-button2, Raise the window to the top of the stack... useless FIXME could find something useful here
      , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
      -- mod-button3, Set the window to floating mode and resize by dragging
      , ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
  ]

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False
