{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses, FlexibleContexts #-}

import Data.Bits
import Data.List
import Control.Monad (liftM, filterM)

-- Import stuff
import XMonad
-- import XMonad.Config.Kde
import XMonad.Config.Desktop
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import XMonad.Util.EZConfig--(additionalKeys)
import System.Exit
import Graphics.X11.Xlib
import System.IO
import Data.Ratio ((%))

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Actions.DynamicWorkspaces
import XMonad.Hooks.FadeInactive

-- actions
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleWindows
import XMonad.Actions.WindowGo
import XMonad.Actions.Navigation2D
import qualified XMonad.Actions.Search as S
import XMonad.Actions.Search
import qualified XMonad.Actions.Submap as SM
import XMonad.Actions.GridSelect
import XMonad.Actions.SpawnOn
import XMonad.Core as XMonad hiding
	(focusFollowsMouse)
import XMonad.Actions.UpdatePointer
import XMonad.Actions.TopicSpace
import XMonad.Actions.WithAll
import XMonad.Actions.GroupNavigation
import XMonad.Layout.PerWorkspace
import XMonad.Prompt.Window
import XMonad.Actions.RotSlaves

-- utils
-- -- (scratchpadSpawnAction, scratchpadManageHook, scratchpadFilterOutWorkspace)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run(spawnPipe)
import qualified XMonad.Prompt as P
import XMonad.Prompt.Shell
import XMonad.Prompt

-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers
-- import XMonad.Hooks.ScreenCorners

-- layouts
import XMonad.Layout.SimplestFloat
import XMonad.Layout.NoBorders
import XMonad.Layout.Accordion
--import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
--import XMonad.Layout.IM
import XMonad.Layout.Tabbed
import XMonad.Layout.MultiToggle
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Grid
import XMonad.Layout.Spacing
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
-- import XMonad.Layout.ToggleLayouts
import XMonad.Layout.LayoutHints
import XMonad.Layout.MagicFocus
import XMonad.Layout.IM
import XMonad.Layout.LayoutModifier
import XMonad.Util.WindowProperties
import XMonad.Layout.EqualSpacing

main = xmonad $ ewmh desktopConfig
  {   keys              = myKeys
    , terminal          = myTerminal
    , workspaces        = myTopics
    , layoutHook        = myLayouts
    , logHook           = myLogHook
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

myLayouts = id . noBorders . mkToggle (NOBORDERS ?? FULL ?? EOT) $ avoidStruts $ equalSpacing 30 0 0 5 $
    onWorkspaces ["1"] mediaLayouts $
    onWorkspaces ["3"] weAllFloatDownHere $
    onWorkspaces ["6", "7", "8"] workLayouts $
    onWorkspaces ["4", "5"] browsersLayouts $
    onWorkspaces ["9"] imLayouts $
    defLayouts
  where
     workLayouts        = (magicFocus $ Mirror wtiled) ||| (magicFocus $ wtiled) ||| Mirror tiled ||| tiled
     defLayouts         = tiled ||| (magicFocus $ Mirror wtiled) ||| (magicFocus $ wtiled) ||| Mirror tiled

     imLayouts          = reflectHoriz $ withIMs (1/6) rosters $ Tall 0 delta ratio
     rosters            = [pidginRoster]
     pidginRoster       = And (ClassName "Pidgin") (Role "buddy_list")
     telRoster          = And (ClassName "Ktp-contactlist") (Role "MainWindow")
     weAllFloatDownHere = simplestFloat ||| Mirror accor
     browsersLayouts    = (Mirror accor) ||| (magicFocus $ wtiled) ||| accor ||| tiled ||| (magicFocus $ Mirror wtiled) ||| Mirror tiled -- not that I ever use anything other than mirror accor...
     mediaLayouts       = (magicFocus $ Mirror wtiled) ||| (magicFocus $ wtiled)
     -- default tiling algorithm partitions the screen into two panes
     tiled              = layoutHints $ Tall nmaster delta ratio
     wtiled             = layoutHints $ Tall nmaster delta (4/5)
     imtiled            = layoutHints $ Tall 2 delta (4/5)
     accor              = Accordion
     -- The default number of windows in the master pane
     nmaster            = 1
     -- Default proportion of screen occupied by master pane
     ratio              = 2/3
     -- Percent of screen to increment by when resizing panes
     delta              = 3/100

myBorderWidth = 0

myTerminal :: String
myTerminal = "st"
-- myTerminal = "rxvtc"
myBrowser  = "firefox"

spawnTS name = spawn $ "LOAD_TMUX_SESSION=" ++ name ++ " " ++ myTerminal

myTopics :: [Topic]
myTopics = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

myTopConf :: TopicConfig
myTopConf = defaultTopicConfig
  {   topicDirs = M.fromList [(show i, "~/") | i <- [1..9]]
    , defaultTopic = "1"
    , defaultTopicAction = const $ return ()
    , topicActions = M.fromList $
      [   ("1", spawnTS "gentoo")
        , ("3", spawnHere "~/local/tor-browser_en-US/Browser/start-tor-browser")
        , ("4", spawnHere "firefox -P uman")
        , ("8", spawnTS "umanlife")
        , ("9", spawnTS "chat" >> spawnHere "pidgin")
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


myLogHook :: X ()
myLogHook = -- updatePointer (Relative 0.5 0.5)
            -- >> fadeInactiveLogHook 0.7
            historyHook
            >> (fadeOutLogHook $ fadeIf (isUnfocused <||> (className =? "Conky") <||> (className =? "lxqt-notificationd")) 0.5)
            -- >> fadeInactiveLogHook 0.8


myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

ks conf@(XConfig {XMonad.modMask = modm}) = [
    -- terminal
    -- fixme: nothing on D! xk_d, B neither. b & d & g v & 0 & shift v & shift 0 & shift t, shift z, shift period for something fun.
    ((modm, xK_Return),                     spawnHere myTerminal)
  , ((modm .|. shiftMask,   xK_Return),     spawnHere $ myTerminal ++ " -e tmux")
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
    ---------
      -- Move focus to the master window
  , ((modm,                 xK_m),          windows W.swapMaster)
    --, ((modm .|. shiftMask, xK_m), sendMessage (Toggle SMARTBORDERS)) -- ; todo:change: toggle max
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
    , ((modm, xK_a), SM.submap . M.fromList $
      [ ((0, xK_c), spawnHere "calibre")
        , ((0, xK_p), spawnHere "pavucontrol-qt")
        , ((0, xK_w), spawnHere "wireshark")
        , ((0, xK_t), spawnHere "transmission-qt")
        , ((0, xK_s), SM.submap . M.fromList $
          [ ((0, xK_c), spawnTS "clj")
            , ((0, xK_u), spawnTS "umanlife")
            , ((0, xK_w), spawnTS "umanlife")
            , ((0, xK_g), spawnTS "gentoo")
          ])
            , ((0, xK_b), SM.submap . M.fromList $
              [ ((0, xK_q), spawnHere "qutebrowser")
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
    , ((modm .|. shiftMask, xK_q),       spawn "qdbus org.kde.ksmserver /KSMServer logout -1 -1 -1") -- FIXME
    -- Restart xmonad
    , ((modm,               xK_q),       spawn "xmonad --recompile; xmonad --restart")
    -- group nav: useless shite.
    , ((modm,               xK_0),       nextMatch Forward (className =? myTerminal))
    , ((modm .|. shiftMask, xK_0),       nextMatch Backward (className =? myTerminal))
    , ((modm,               xK_v),       nextMatchOrDo Forward (className =? "Gvim") (spawnHere "~/local/bin/gvim"))
    , ((modm .|. shiftMask, xK_v),       nextMatchOrDo Backward (className =? "Gvim") (spawnHere "~/local/bin/gvim"))
    -- launch stuff!
    , ((modm,               xK_z),       spawn "xscreensaver-command --lock")
    , ((modm .|. shiftMask, xK_i),       spawnHere "urxvt")
  -- , ((modm .|. shiftMask, xK_period),  spawnTS "clj") -- not sure 
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
    , ((0,                  0x1008ff02), spawn "xbacklight -inc 10")
    , ((0,                  0x1008ff03), spawn "xbacklight -dec 10")
    -- should be kill wifi, suspend for me:
    --, ((0, 0x1008ff95), spawn "systemctl suspend")
    -- 0x1008ff81, should be XF86Tools, hybrid sleep:
    , ((modm .|. shiftMask, xK_F10),     spawn "systemctl hibernate")
    -- XF86Search suspend:
    , ((modm .|. shiftMask, xK_F11),     spawn "systemctl hybrid-sleep")
    -- 0x1008ff4a, XF86LaunchA
    -- 0x1008ff4a
    , ((modm .|. shiftMask, xK_F12),     spawn "systemctl suspend")
                                            ]
  ++
    [((modm, k), goToTopic $ show i) | (i, k) <- zip [1..9] [xK_1..xK_9]]
  ++
    [((modm .|. shiftMask, k), windows $ W.shift $ show i) | (i, k) <- zip [1..9] [xK_1..xK_9]]

modalmode conf@(XConfig {XMonad.modMask = modm}) = [ ((m `xor` modm, k), a >> (SM.submap . M.fromList $ modalmode conf))
                                                   | ((m, k), a) <- ks conf ]

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $ ((modm, xK_n), SM.submap . M.fromList $ modalmode conf) : ks conf

-- TODO
--	urgency hook --> irssi





-- modified version of XMonad.Layout.IM --
--
-- see: https://wiki.haskell.org/Xmonad/Config_archive/Thomas_ten_Cate's_xmonad.hs
--
-- | Data type for LayoutModifier which converts given layout to IM-layout
-- (with dedicated space for the roster and original layout for chat windows)
data AddRosters a = AddRosters Rational [Property] deriving (Read, Show)
 
instance LayoutModifier AddRosters Window where
  modifyLayout (AddRosters ratio props) = applyIMs ratio props
  modifierDescription _                = "IMs"
 
-- | Modifier which converts given layout to IMs-layout (with dedicated
-- space for rosters and original layout for chat windows)
withIMs :: LayoutClass l a => Rational -> [Property] -> l a -> ModifiedLayout AddRosters l a
withIMs ratio props = ModifiedLayout $ AddRosters ratio props
 
-- | IM layout modifier applied to the Grid layout
gridIMs :: Rational -> [Property] -> ModifiedLayout AddRosters Grid a
gridIMs ratio props = withIMs ratio props Grid
 
hasAnyProperty :: [Property] -> Window -> X Bool
hasAnyProperty [] _ = return False
hasAnyProperty (p:ps) w = do
    b <- hasProperty p w
    if b then return True else hasAnyProperty ps w

applyIMs :: (LayoutClass l Window) =>
               Rational
            -> [Property]
            -> W.Workspace WorkspaceId (l Window) Window
            -> Rectangle
            -> X ([(Window, Rectangle)], Maybe (l Window))
applyIMs ratio props wksp rect = do
    let stack                    = W.stack wksp
    let ws                       = W.integrate' $ stack
    rosters                      <- filterM (hasAnyProperty props) ws
    let n                        = fromIntegral $ length rosters
    let m                        = round n
    let (rostersRect, chatsRect) = splitHorizontallyBy (n * ratio) rect
    let rosterRects              = splitHorizontally m rostersRect
    --let rosterRects = splitHorizontally n rostersRect
    let filteredStack            = stack >>= W.filter (`notElem` rosters)
    (a,b)                        <- runLayout (wksp {W.stack = filteredStack}) chatsRect
    return (zip rosters rosterRects ++ a, b)

-- imLayout = avoidStruts $ reflectHoriz $ withIMs ratio rosters chatLayout where
--     chatLayout      = Grid
--     ratio           = 1%6
--     rosters         = [skypeRoster, pidginRoster, telRoster]
--     pidginRoster    = And (ClassName "Pidgin") (Role "buddy_list")
--     skypeRoster     = (ClassName "Skype") `And` (Not (Title "Options")) `And` (Not (Role "Chats")) `And` (Not (Role "CallWindowForm"))
--     telRoster	    = And (Role "ktp-contactlist") (ClassName "MainWindow")
--  
-- myLayoutHook = fullscreen $ im $ normal where
--     normal     = tallLayout ||| wideLayout ||| singleLayout
--     fullscreen = onWorkspace "fullscreen" fullscreenLayout
--     im         = onWorkspace "im" imLayout
--  
-- -- special treatment for specific windows:
-- -- put the Pidgin and Skype windows in the im workspace
-- myManageHook = imManageHooks <+> manageHook myBaseConfig
-- imManageHooks = composeAll [isIM --> moveToIM] where
--     isIM     = foldr1 (<||>) [isPidgin, isSkype]
--     isPidgin = className =? "Pidgin"
--     isSkype  = className =? "Skype"
--     moveToIM = doF $ W.shift "im"

