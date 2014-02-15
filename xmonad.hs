{-# LANGUAGE DeriveDataTypeable #-}

import Data.Bits
import Control.Monad (liftM)

-- Import stuff
import XMonad
import XMonad.Config.Kde
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import XMonad.Util.EZConfig(additionalKeys)
import System.Exit
import Graphics.X11.Xlib
import System.IO

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Actions.DynamicWorkspaces
import XMonad.Hooks.FadeInactive

-- actions
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleWindows
import XMonad.Actions.WindowGo
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
import XMonad.Layout.NoBorders
import XMonad.Layout.Accordion
--import XMonad.Layout.ResizableTile
--import XMonad.Layout.Reflect
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

main = do
	xmonad $ ewmh kdeConfig
		{ keys			= myKeys
		, terminal		= myTerminal
		, workspaces		= myTopics
		, layoutHook		= myLayouts
		, logHook		= myLogHook
		-- , startupHook		= myStartupHook
		-- , handleEventHook	= myEventHook
		, focusFollowsMouse	= myFocusFollowsMouse
		, borderWidth		= myBorderWidth
		, manageHook		= composeOne
		        [
			-- transience
			  resource  =? "desktop_window" -?> doFloat
			, resource  =? "trayer"         -?> doFloat
			, isKDETrayWindow               -?> doFloat
			, resource  =? "kdesktop"       -?> doFloat
			, resource  =? "Dialog"         -?> doFloat
			]
			<+> composeAll
			[ manageDocks
			-- , className =? "krunner" --> doIgnore
			-- , resource =? "Conky" --> doIgnore
			, namedScratchpadManageHook scratchpads ]
		}


--myNoSpacing :: Int -> l a -> ModifiedLayout Spacing l a
--myNoSpacing = (spacing 0)
--
--data NOSPACING = NOSPACING deriving (Read, Show, Eq, Typeable)
--instance Transformer NOSPACING Window where
--	transform _ x k = k (myNoSpacing x) (\(myNoSpacing x') -> x')

--data BORDERS = BORDERS deriving (Read, Show, Eq, Typeable)
--instance Transformer BORDERS Window where
--	transform _ x k = k (Borders x) (\(Borders x') -> x')


--data NOSPACING = NOSPACING deriving (Read, Show, Eq, Typeable)
--instance Transformer NOSPACING Window where
--	transform _ x k = k (spacing 0 x) (\(spacing Int x') -> x')
--	$ avoidStruts $ tiled ||| (magicFocus $ Mirror tiled) ||| Mirror accor ||| XMonad.Layout.Grid.Grid

myLayouts = id . noBorders . mkToggle (NOBORDERS ?? FULL ?? EOT) $ avoidStruts $ spacing 15 $
	    onWorkspaces ["6", "7", "8"] workLayouts $
	    onWorkspace "9" imLayouts $
	    onWorkspace "1" mediaLayouts $
	    defLayouts
  where
     workLayouts  = (magicFocus $ Mirror wtiled) ||| Mirror accor ||| wtiled
     defLayouts   = tiled ||| (magicFocus $ Mirror wtiled) ||| Mirror accor
     imLayouts    = imtiled ||| (magicFocus $ Mirror wtiled) ||| Mirror accor
     mediaLayouts = Mirror accor ||| tiled ||| (magicFocus $ Mirror wtiled)
     -- default tiling algorithm partitions the screen into two panes
     tiled    = Tall nmaster delta ratio
     wtiled   = Tall nmaster delta (4/5)
     imtiled  = Tall nmaster delta (4/5)
     accor    = Accordion
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 2/3
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

-- hack; this is because layout defaults don't apply to NSP
myBorderWidth = 0

myTerminal :: String
myTerminal = "terminology"
-- myTerminal = "urxvtc"
myBrowser  = "firefox"

spawnTS name = spawn $ "LOAD_TMUX_SESSION=" ++ name ++ " " ++ myTerminal

myTopics :: [Topic]
myTopics = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

myTopConf :: TopicConfig
myTopConf = defaultTopicConfig
  { topicDirs = M.fromList [(show i, "~/") | i <- [1..9]]
  , defaultTopic = "1"
  , defaultTopicAction = const $ return ()
  , topicActions = M.fromList $
    [ ("1", spawnTS "def-gtoo")
    --, ("2", spawn "gnome-control-center sound")
    , ("3", spawn "firefox")
    , ("4", spawn "kontact")
    -- , ("6", spawnTS "clj")
    --, ("7", spawnTS "mix")
    , ("8", spawnTS "aqua")
    , ("9", spawn "kopete")
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
	    >> (fadeOutLogHook $ fadeIf ((isUnfocused) <||> (className =? "Conky")) 0.7)
            -- >> fadeInactiveLogHook 0.8


myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- scratch pads;
scratchpads = [ NS "notes" "~/local/bin/gvim --role notes ~/TODO" (role =? "notes")
	          (customFloating $ W.RationalRect (1/8) (1/8) (6/8) (6/8))
              -- , NS "conky" "conky" (className =? "Conky")
	      --     (customFloating $ W.RationalRect 0 0 1 1 )
	      ] where role = stringProperty "WM_WINDOW_ROLE"

ks conf@(XConfig {XMonad.modMask = modm}) = [
    -- terminal
      ((modm, xK_Return                  ), spawnTS "def-gtoo")
    , ((modm .|. shiftMask, xK_Return    ), spawn myTerminal)
    -- close focused window
    , ((modm .|. shiftMask, xK_c         ), kill)
    ---------
    -- Rotate through the available layout algorithms
    , ((modm,               xK_Tab       ), sendMessage NextLayout)
    --  Reset the layouts on the current workspace to default
    , ((modm  .|. shiftMask, xK_Tab      ), setLayout $ XMonad.layoutHook conf)
    ---------
    -- Shrink/expand the master area
    , ((modm,               xK_u         ), sendMessage Shrink)
    , ((modm,               xK_i         ), sendMessage Expand)
    ---------
    -- send window to next WS
    , ((modm .|. shiftMask, xK_h         ), shiftToPrev >> prevTS)
    , ((modm .|. shiftMask, xK_l         ), shiftToNext >> nextTS)
    -- next WS
    , ((modm,               xK_h         ), prevTS)
    , ((modm,               xK_l         ), nextTS)
    ---------
    , ((modm,               xK_space     ), toggleTopics)
    , ((modm .|. shiftMask, xK_space     ), nextMatch History (return True))
    --, ((modm            ,  xK_BackSpace  ), cycleRecentWindows [xK_Alt_L] xK_j xK_k)
    --, ((modm            ,  xK_BackSpace  ), goToSelected gridselectWindow)
    --, ((modm            ,  xK_BackSpace  ), nextMatch History (return True))
    , ((modm,               xK_y         ), currentTopicAction myTopConf)
    -- Move focus to the next window
    , ((modm,               xK_j         ), windows W.focusDown)
    -- Move focus to the previous window
    , ((modm,               xK_k         ), windows W.focusUp)
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j         ), windows W.swapDown)
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k         ), windows W.swapUp)
    ---------
    -- Move focus to the master window
    , ((modm,               xK_m         ), windows W.swapMaster)
  --, ((modm .|. shiftMask, xK_m         ), sendMessage (Toggle SMARTBORDERS)) -- ; todo:change: toggle max
    , ((modm,               xK_f         ), sendMessage (Toggle FULL)) -- ; todo:change: toggle max
    -- Increment the number of windows in the master area
    , ((modm,               xK_comma     ), sendMessage (IncMasterN 1))
    , ((modm,               xK_period    ), sendMessage (IncMasterN (-1)))
	---------
	-- Toggle the status bar gap --  Use this binding with avoidStruts from Hooks.ManageDocks.
    , ((modm,               xK_b         ),  sendMessage  ToggleStruts)
    , ((modm .|. shiftMask, xK_b         ),  SM.submap . M.fromList $
		    [ ((0, xK_c),     spawn "chromium")
		    , ((0, xK_f),     spawn "firefox")
		    , ((0, xK_o),     spawn "opera")])
	-- Push window back into tiling
    , ((modm,               xK_t         ), sinkAll)
	--------- reset mouse pointer
    ,  ((modm               ,  xK_z      ),  updatePointer $ Relative 0.5 0.5) -- nope
	-- Quit xmonad
-- ,  ((modm .|. shiftMask ,  xK_q      ), io (exitWith ExitSuccess))
    ,  ((modm .|. shiftMask ,  xK_q      ), spawn "qdbus org.kde.ksmserver /KSMServer logout -1 -1 -1")
    -- Restart xmonad
    ,  ((modm               , xK_q       ),  spawn  "xmonad  --recompile; xmonad --restart")
    ---------
    -- launch stuff!
    ,  ((modm               ,  xK_slash  ),  namedScratchpadAction scratchpads "notes")
    ,  ((modm               ,  xK_v      ),  spawn "~/local/bin/gvim")
    -- ,  ((modm .|. shiftMask ,  xK_g      ),  spawn "pidgin")
    ,  ((modm .|. shiftMask ,  xK_z      ),  spawn "xscreensaver-command --lock")
    ,  ((modm .|. shiftMask ,  xK_i      ),  spawn "urxvtc")
    ,  ((modm .|. shiftMask ,  xK_period ),  spawnTS "clj")
    ,  ((modm .|. shiftMask ,  xK_t      ),  spawn "transmission-qt")
    ,  ((modm .|. shiftMask,   xK_m	 ),  spawn "exaile") -- ; todo:change: toggle max
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
