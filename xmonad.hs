{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses, FlexibleContexts #-}

import Data.Bits
import Data.List
import Control.Monad (liftM, filterM, join)
import Network.HostName

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
import XMonad.Core as XMonad
import XMonad.Hooks.FadeInactive

-- actions
import qualified XMonad.Actions.Search as S
import qualified XMonad.Actions.Submap as SM
import XMonad.Actions.CycleWindows
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GridSelect
import XMonad.Actions.GroupNavigation
import XMonad.Actions.Navigation2D
import XMonad.Actions.OnScreen
import XMonad.Actions.RotSlaves
import XMonad.Actions.SpawnOn
import XMonad.Actions.TopicSpace
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowGo
import XMonad.Actions.WithAll

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
import XMonad.MyStuff.AddRosters

main = do
  toggleFadeSet <- newIORef DS.empty
  hostname      <- getHostName
  xmonad $ ewmh desktopConfig
    { keys              = myKeys hostname toggleFadeSet
    , mouseBindings     = myMouseBindings
    , terminal          = myTerminal
    , workspaces        = myTopics
    , layoutHook        = myLayouts hostname
    , modMask           = mod4Mask
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

myLayouts hostname =
    init $
    modWorkspaces [[x] | x <- ['1'..'9']] (spacing $ borders hostname "screen1") $
    onWorkspaces ["1"] workLayouts $
    onWorkspaces ["2"] imTooSquare $
    onWorkspaces ["3"] weAllFloatDownHere $ -- not sure if I'm keeping this.
    onWorkspaces ["4", "5"] browsersLayouts $
    onWorkspaces ["6", "7", "8"] workLayouts $
    onWorkspaces ["9"] imLayouts $
    -- second monitor:
    modWorkspaces [['1', x] | x <- ['1'..'9']] (spacing $ borders hostname "screen2") $
    onWorkspaces ["11"] workLayouts $
    onWorkspaces ["12"] imTooSquare $
    onWorkspaces ["13"] weAllFloatDownHere $
    onWorkspaces ["14", "15"] browsersLayouts $
    onWorkspaces ["16", "17", "18"] workLayouts $
    onWorkspaces ["19"] imLayouts $
    defLayouts
  where
    -- FIXME try XMonad.Layout.BinarySpacePartition
    borders "yggdrasill" "screen1" = Border 20 20 20 20
    borders "yggdrasill" "screen2" = Border 10 10 10 10
    borders "daban-urnud" _        = Border 10 10 10 10
    borders _  _                   = Border 10 10 10 10
    init args                      = noBorders . mkToggle (NOBORDERS ?? FULL ?? EOT) $ avoidStruts args
    spacing bd                     = spacingRaw False bd True bd True
    --
    workLayouts                    = magicFocus (Mirror wtiled) ||| magicFocus wtiled ||| Mirror wtiled ||| wtiled
    defLayouts                     = tiled ||| magicFocus (Mirror wtiled) ||| magicFocus wtiled ||| Mirror tiled
    imLayouts                      = reflectHoriz $ withIMs (1/6) rosters $ Tall 0 delta ratio
    rosters                        = [pidginRoster, gajimRoster]
    pidginRoster                   = And (ClassName "Pidgin") (Role "buddy_list")
    telRoster                      = And (ClassName "Ktp-contactlist") (Role "MainWindow")
    gajimRoster                    = And (ClassName "Gajim") (Role "roster")
    weAllFloatDownHere             = simplestFloat ||| Accordion
    imTooSquare                    = Grid ||| Mirror zoomRow
    browsersLayouts                = Mirror Accordion ||| magicFocus wtiled ||| Accordion ||| tiled ||| magicFocus (Mirror wtiled) ||| Mirror tiled -- not that I ever use anything other than mirror accor...
    mediaLayouts                   = magicFocus (Mirror wtiled) ||| magicFocus wtiled
    -- default tiling algorithm partitions the screen into two panes
    tiled                          = layoutHints $ Tall nmaster delta ratio
    wtiled                         = layoutHints $ Tall nmaster delta (4/5)
    imtiled                        = layoutHints $ Tall 2 delta (4/5)
    -- The default number of windows in the master pane
    nmaster                        = 1
    -- Default proportion of screen occupied by master pane
    ratio                          = 2/3
    -- Percent of screen to increment by when resizing panes
    delta                          = 3/100

myBorderWidth = 0

myTerminal :: String
myTerminal = "~/conf/misc/scripts/st.sh"
myChat = "gajim"

-- myChat "yggdrasill"  = "GDK_SCALE=3 GDK_DPI_SCALE=0.4 gajim"
-- myChat "daban-urnud" = "GDK_SCALE=2 GDK_DPI_SCALE=0.5 gajim"

myBrowser "daban-urnud" = "firefox"
myBrowser _             = "firefox"

spawnRemoteSessions "yggdrasill"  =  spawnRemoteTmuxSession "wg.nostromo.local" "gentoo"
                                  >> spawnRemoteTmuxSession "wg.undefined.local" "gentoo"
                                  >> spawnRemoteTmuxSession "5.39.77.155" "gentoo"
                                  >> spawnRemoteTmuxSession "wg.daban-urnud.local" "remote"
spawnRemoteSessions "daban-urnud" =  spawnRemoteTmuxSession "wg.nostromo.local" "gentoo"
                                  >> spawnRemoteTmuxSession "wg.undefined.local" "gentoo"
                                  >> spawnRemoteTmuxSession "5.39.77.155" "gentoo"
                                  >> spawnRemoteTmuxSession "wg.yggdrasill.local" "remote"
spawnRemoteSessions _             =  spawnRemoteTmuxSession "wg.nostromo.local" "gentoo"


xReset = "pactl set-sink-volume 0 30%; pactl set-sink-volume 1 20%; pactl set-sink-mute 1 true; pactl set-sink-mute 0 true; " -- reset sound
      ++ "setxkbmap dvorak; ~/conf/misc/scripts/kbd.sh; " -- reset kbd FIXME should be done on wake up!

spawnTmuxSession name               = spawn $ "LOAD_TMUX_SESSION=" ++ name ++ " " ++ myTerminal
spawnRemoteTmuxSession host session = spawn $ myTerminal ++ " -e ssh -t " ++ host ++ " LOAD_TMUX_SESSION=" ++ session ++ " zsh"
spawnCmd cmd                        = spawn $ myTerminal ++ " -e " ++ cmd

defaultSession "daban-urnud" "1" = "media"
defaultSession "daban-urnud" "8" = "gentoo"
defaultSession "yggdrasill"  "1" = "gentoo"
defaultSession "yggdrasill"  "8" = "2m"
defaultSession _             "1" = "gentoo"
defaultSession _             "8" = "reader"

myTopics :: [Topic]
myTopics = [[x] | x <- ['1'..'9']] ++ [['1', x] | x <- ['1'..'9']] -- "1" --> "19", skipping 10 & 0
myTopConf :: String -> TopicConfig
myTopConf hostname = def
  { topicDirs = M.fromList [(show i, "~/") | i <- [1..9] ++ [11 .. 19]]
  , defaultTopic = "1"
  , defaultTopicAction = const $ return ()
  , topicActions = M.fromList
      [ ("1", spawnTmuxSession $ defaultSession hostname "1")
      , ("2", spawnRemoteSessions hostname)
      , ("3", spawnHere "~/local/tor-browser_en-US/Browser/start-tor-browser")
      , ("4", spawnHere $ myBrowser hostname ++ " -P uman")
      , ("8", spawnTmuxSession $ defaultSession hostname "8")
      , ("9", spawnHere myChat)
      , ("11", spawnTmuxSession "logs")
      , ("12", spawnRemoteSessions hostname)
      , ("13", spawnHere "~/local/tor-browser_en-US/Browser/start-tor-browser")
      , ("14", spawnHere $ myBrowser hostname ++ " -P small")
      , ("17", spawnTmuxSession "2mp")
      --, ("17", spawnHere $ myTerminal ++ " -e tmux")

      , ("18", spawn "VIM_SERVER=DANCE_COMMANDER ~/conf/misc/scripts/nvim.sh")
      , ("19", spawnTmuxSession "chat")
      ]
  }

myLogHook :: IORef (DS.Set Window) -> X ()
myLogHook toggleFadeSet = historyHook >> fadeOutLogHook (fadeIf (fadeCondition toggleFadeSet) 0.8)

fadeCondition :: IORef (DS.Set Window) -> Query Bool
fadeCondition floats =
  (isUnfocused <||> (className =? "Conky"))
  <&&> (join . asks $ \w -> liftX . io $ DS.notMember w `fmap` readIORef floats)

toggleFadeOut :: Window -> DS.Set Window -> DS.Set Window
toggleFadeOut w s | w `DS.member` s = DS.delete w s
                  | otherwise = DS.insert w s

ks hostname toggleFadeSet conf@XConfig {XMonad.modMask = modm} = [
    -- FIXME: nothing on D! xk_d, B neither.
    -- b, d, g, v, V, 0, s, v, i, A, q backspace   shift v, shift 0, shift t, shift z, shift period for something fun.
    --
    -- terminal stuff:
    ((modm, xK_Return),                     spawnHere $ myTerminal ++ " -e tmux")
  , ((modm .|. shiftMask,   xK_Return),     spawnHere myTerminal)
  , ((modm .|. shiftMask,   xK_i),          spawnHere "urxvt") -- fallback term: FIXME use for something else.

  , ((modm,                 xK_r),          spawnHere "rofi -combi-modi window,drun,ssh -theme lb -font \"fira 30\" -show combi") -- themes: gruvbox-dark-soft, lb, Paper, solarized_alternate
    -- workspace/layout stuff:
  , ((modm,                 xK_Tab),        sendMessage NextLayout)
  , ((modm .|. shiftMask,   xK_Tab),        setLayout $ XMonad.layoutHook conf) -- reset layouts
  , ((modm .|. shiftMask,   xK_h),          shiftToPrev >> prevWS)
  , ((modm .|. shiftMask,   xK_l),          shiftToNext >> nextWS)
  , ((modm,                 xK_h),          prevWS)
  , ((modm,                 xK_l),          nextWS)
  , ((modm,                 xK_space),      toggleWS)
--, ((modm .|. shiftMask,   xK_space),      nextScreen) -- FIXME use for something else
  , ((modm .|. controlMask, xK_space),      nextScreen) -- toggle screens/monitors
  , ((modm,                 xK_u),          sendMessage Shrink) -- master size
  , ((modm,                 xK_i),          sendMessage Expand)
  , ((modm,                 xK_comma),      sendMessage (IncMasterN 1)) -- nb windows in master
  , ((modm,                 xK_period),     sendMessage (IncMasterN $ -1))
  , ((modm,                 xK_f),          sendMessage (Toggle FULL))
  , ((modm,                 xK_m),          windows W.swapMaster)
  , ((modm,                 xK_y),          currentTopicAction $ myTopConf hostname)
  , ((modm .|. shiftMask,   xK_b),          sendMessage ToggleStruts) -- Toggle the status bar gap -- Use this binding with avoidStruts from Hooks.ManageDocks.
  , ((modm,                 xK_t),          sinkAll) --  Push windows back into tiling
    -- window stuff:
  , ((modm .|. shiftMask,   xK_c),          kill)
  , ((modm,                 xK_c),          spawn "~/conf/misc/scripts/dunst.sh close")
  , ((modm,                 xK_j),          windows W.focusDown)
  , ((modm,                 xK_k),          windows W.focusUp)
  , ((modm .|. shiftMask,   xK_j),          windows W.swapDown)
  , ((modm .|. shiftMask,   xK_k),          windows W.swapUp)
  , ((modm .|. controlMask, xK_j),          windowGo D False)  -- 2d nav:
  , ((modm .|. controlMask, xK_k),          windowGo U False)
  , ((modm .|. controlMask, xK_h),          windowGo L False)
  , ((modm .|. controlMask, xK_l),          windowGo R False)
  , ((modm,                 xK_q),          spawn "xmonad --recompile; xmonad --restart") -- FIXME: do something else?
  -- , ((modm .|. shiftMask, xK_v),       nextMatchOrDo Backward (className =? "Gvim") (spawnHere "~/local/bin/gvim")) -- this exits, might use this someday??
    -- mediakeys / hotkeys:
  , ((0, 0x1008ff12), spawn "pactl set-sink-mute 0 toggle; pactl set-sink-mute 1 toggle")-- XF86AudioMute
  , ((0, 0x1008ff13), spawn "pactl set-sink-volume 0 +10%; pactl set-sink-volume 1 +10%") -- "XF86AudioRaiseVolume"
  , ((0, 0x1008ff11), spawn "pactl set-sink-volume 0 -10%; pactl set-sink-volume 1 -10%") -- XF86AudioLowerVolume
  , ((0, 0x1008ffb2), spawn "pactl set-source-mute 0 toggle; pactl set-source-mute 1 toggle") -- toggle mic
  , ((0, 0x1008ff02), spawn "light -A 10") -- brightness:
  , ((0, 0x1008ff03), spawn "light -U 10")

  -- launch stuff!
  , ((modm, xK_a), SM.submap . M.fromList $
    [ ((0, xK_c), spawnHere "calibre")
    , ((0, xK_d), spawnHere "dolphin")
    , ((0, xK_g), spawnHere "gimp")
    , ((0, xK_p), spawnHere "pavucontrol-qt")
    , ((0, xK_w), spawnHere "wireshark")
    , ((0, xK_t), spawnHere "transmission-qt")
    -- tmux sessions:
    , ((modm, xK_s), SM.submap . M.fromList $
      [ ((0, xK_c), spawnTmuxSession "chat")
      , ((0, xK_w), spawnTmuxSession "2m")
      , ((0, xK_g), spawnTmuxSession "gentoo")
      , ((0, xK_m), spawnTmuxSession "media")
      ])
    -- browsers:
    , ((modm, xK_b), SM.submap . M.fromList $
      [ ((0, xK_q),         spawnHere "qutebrowser")
      , ((0,         xK_c), spawnHere "chromium")
      , ((0,         xK_g), spawnHere "google-chrome-stable")
      , ((0,         xK_f), spawnHere $ myBrowser hostname ++ " -P uman")
      , ((shiftMask, xK_f), spawnHere $ myBrowser hostname ++ " --ProfileManager --new-instance")
      , ((0,         xK_o), spawnHere "opera")
      , ((0,         xK_t), spawnHere "~/local/tor-browser_en-US/Browser/start-tor-browser")
      ])
    -- chats:
    , ((modm, xK_c), SM.submap . M.fromList $
      [ ((0, xK_c), spawnTmuxSession "chat")
      , ((0, xK_p), spawnHere "pidgin")
      , ((0, xK_g), spawnHere "gajim")
      ])
    -- terms:
    , ((modm, xK_t), SM.submap . M.fromList $
      [ ((0, xK_t), spawnHere "terminology")
      , ((0, xK_x), spawnHere "xterm")
      , ((0, xK_u), spawnHere "urxvt")
      , ((0, xK_s), spawnHere "st")
      ])
    ])
  -- random things:
  , ((modm, xK_z), SM.submap . M.fromList $
    -- session locking:
    [ ((modm,      xK_z),          spawn "xscreensaver-command --lock || (xscreensaver -no-splash&) && sleep 0.5 && xscreensaver-command -lock")
    , ((0,         xK_s),          spawn $ xReset ++ "systemctl suspend")
    , ((shiftMask, xK_s),          spawn $ xReset ++ "systemctl hybrid-sleep")
    , ((shiftMask, xK_h),          spawn $ xReset ++ "systemctl hibernate")

    -- window borders:
    , ((0,         xK_e),          decScreenWindowSpacing 10)
    , ((0,         xK_u),          incScreenWindowSpacing 10)
    , ((0,         xK_i),          toggleWindowSpacingEnabled >> toggleScreenSpacingEnabled)

    -- dunst:
    , ((0,         xK_c),          spawn "~/conf/misc/scripts/dunst.sh close_all")
    , ((0,         xK_h),          spawn "~/conf/misc/scripts/dunst.sh history")
    -- pause and resume dunst notifs
    , ((0,         xK_quoteright), spawn "killall -SIGUSR1 dunst")
    , ((shiftMask, xK_quoteright), spawn "killall -SIGUSR2 dunst")

    -- misc:
    , ((0,         xK_q),          spawn "xmonad --recompile; xmonad --restart")
    , ((0,         xK_k),          spawn xReset)
    , ((0,         xK_f),          withFocused $ io . modifyIORef toggleFadeSet . toggleFadeOut)
    --, ((0,         xK_s),          shiftNextScreen >> nextScreen) -- TODO time proof this
    ])
  ]
  ++ -- this could be in previous [], but this should be grouped with the next group of keyboard definitions:
  [ ((modm, xK_0), windows (viewOnScreen 1 "18") >> currentTopicAction (myTopConf hostname)) ] -- raise dance commander on external monintor.
  ++
  [((m .|. modm, k), windows $ f i)
    | (i, k) <- zip [[x] | x <- ['1'..'9']] [xK_1..xK_9] -- lest we forget: [x] -> char vs [char] = string in haskell --> "1" through "9"
    , (f, m) <- [ (viewOnScreen 0, 0)
    , (W.shift, shiftMask)]]
  ++
  [((m .|. modm, k), windows $ f i)
    | (i, k) <- zip [['1', x] | x <- ['1'..'9']] [xK_1..xK_9] -- lest we forget: [x] -> char vs [char] = string in haskell --> "11" through "19"
    , (f, m) <- [ (viewOnScreen 1, controlMask)
    , (W.shift, controlMask .|. shiftMask)]]

modalmode hostname toggleFadeSet conf@(XConfig {XMonad.modMask = modm}) = [ ((m `xor` modm, k), a >> (SM.submap . M.fromList $ modalmode hostname toggleFadeSet conf)) | ((m, k), a) <- ks hostname toggleFadeSet conf ]

myKeys :: String -> IORef (DS.Set Window) -> XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys hostname toggleFadeSet conf@(XConfig {XMonad.modMask = modm}) = M.fromList $ ((modm, xK_n), SM.submap . M.fromList $ modalmode hostname toggleFadeSet conf) : ks hostname toggleFadeSet conf

myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList
  [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
  -- mod-button2, Raise the window to the top of the stack... useless FIXME could find something useful here
  , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
  -- mod-button3, Set the window to floating mode and resize by dragging
  , ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
  ]

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False
