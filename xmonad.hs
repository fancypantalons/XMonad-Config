import qualified Data.Map as M

import Control.OldException
import Control.Monad

import DBus
import DBus.Connection
import DBus.Message

import Data.List 
import qualified Data.Set as S

import Foreign.C.Types

import XMonad hiding ( (|||), Tall )
import XMonad.Actions.Plane
import XMonad.Actions.TagWindows
import XMonad.Actions.CopyWindow
import XMonad.Config.Gnome
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Layout hiding ( (|||) )
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Named
import qualified XMonad.Layout.HintedTile as H
import XMonad.Layout.Magnifier
import XMonad.Layout.Circle
import XMonad.Layout.LimitWindows
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Tabbed
import XMonad.Layout.IM
import XMonad.Layout.Reflect
import XMonad.Operations
import XMonad.Prompt
import qualified XMonad.StackSet as W

import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad

-- Use XMonad.Actions.CopyWindow.copyWindow to copy the specified window
-- to the current workspace. 
copyHere :: (Ord a, Eq s, Eq i) => a -> W.StackSet i l a s sd -> W.StackSet i l a s sd
copyHere w s = copyWindow w (W.currentTag s) s

-- Pushes the topmost window down in the window stack.  We use this
-- in our manage hooks to force any newly created windows down so they
-- don't displace the master window.
pushTopmostDown :: W.StackSet i l a s sd -> W.StackSet i l a s sd
pushTopmostDown = W.modify' $ modifyFunc
  where modifyFunc c@(W.Stack t [] (r:rs)) = W.Stack t [r] rs
        modifyFunc c@_                     = c

-- Dunno why this isn't standard, but...
roleName :: Query String
roleName = stringProperty "WM_WINDOW_ROLE"

--
-- Returns a query which checks if the window has the given property.
--
hasProperty :: String -> Query Bool
hasProperty name = ask >>= \w -> liftX $ withDisplay $ queryFunc w
  where queryFunc window display = do 
          atom <- getAtom name

          prop8 <- io $ getWindowProperty8 display atom window
          prop16 <- io $ getWindowProperty16 display atom window
          prop32 <- io $ getWindowProperty32 display atom window

          --
          -- This is actually the opposite of the Maybe monad (I want to
          -- *continue* on Nothing), so I can't just use a monad here.
          --
          case prop8 of
            Just x  -> return True
            Nothing ->
              case prop16 of
                Just x  -> return True
                Nothing ->
                  case prop32 of
                    Just x  -> return True
                    Nothing -> return False

-- Use EWMH tags to determine if the window type in question is
-- a splash window or not (among others, this works for Gnome Do).
--
-- The second stanza is a special case for the Eclipse splash screen
-- which decided to do things differently...
isSplash :: Query Bool
isSplash = 
  (isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH") <||>
  ((hasProperty "_MOTIF_WM_HINTS") <&&> (className =? "Eclipse"))

-- Use EWMH tags to determine if the window type in question is
-- a dock window (ie, one put up by a panel).
isDock :: Query Bool
isDock = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DOCK"

-- Matches Evolution windows that are *not* the main evolution window.  We 
-- use this later to float Evolution windows (ie, compose, etc).
isEvolutionChild :: Query Bool
isEvolutionChild = (className =? "Evolution") <&&> (fmap (not . isSuffixOf " - Evolution") title)

-- Matches our scratchpad window.
isScratchpad :: Query Bool
isScratchpad = (className =? "scratchpad")

-- Another rule to identify wikidpad.  Just use the title (unfortunately wx
-- provides no method for setting the window class or role).
isNotepad :: Query Bool
isNotepad = (className =? "WikidPad.py") <&&> (fmap (isSuffixOf "Flotsam.wiki - WikidPad") title)

isKeepass :: Query Bool
isKeepass = fmap (isSuffixOf "- KeePass Password Safe") title

isAndroid :: Query Bool
isAndroid = (className =? "emulator64-arm")

-- And another rule to test for exaile.
isMusicPlayer :: Query Bool
isMusicPlayer = (className =? "Exaile")

-- And a rule for Deluge
isTorrentClient :: Query Bool
isTorrentClient = (className =? "Deluge")

-- Use this to float various FF windows
isFirefoxPrefs :: Query Bool
isFirefoxPrefs = 
  ((roleName =? "Preferences") <||> (roleName =? "Permissions")) <&&> 
  ((className =? "Firefox") <||> (className =? "Iceweasel"))

-- Rule to use when determining if we should fade out a window
isFadeEligable :: Query Bool
isFadeEligable = isUnfocused <&&> (liftM not isSpecialWindow)
  where isSpecialWindow = (className =? "xine") <||> 
                          (className =? "MPlayer") <||>
                          (className =? "Mythfrontend")

-- Wrap the given string in a span tag which changes the font color to the
-- provided color.
colorize :: String -> String -> String
colorize fg str = wrap left right str
  where left  = "<span foreground=\"" ++ fg ++ "\">"
        right = "</span>"

-- Clean up the given string so we can safely send it to our logging applet.
-- We're just stripping high-ASCII and converting them to HTML entities.
sanitize :: String -> String
sanitize [] = []
sanitize (x:rest) 
  | xVal > 127 = "&#" ++ show xVal ++ "; " ++ sanitize rest
  | otherwise  = x : sanitize rest
  where xVal = fromEnum x

-- Look up the logging applet over DBus
getWellKnownName :: Connection -> IO()
getWellKnownName dbus = tryGetName `catchDyn` catchFun
  where catchFun (DBus.Error _ _) = getWellKnownName dbus
        tryGetName = do
          namereq <- newMethodCall serviceDBus pathDBus interfaceDBus "RequestName"
          addArgs namereq [String "org.xmonad.Log", Word32 5]
          sendWithReplyAndBlock dbus namereq 0
          return ()

-- Log the given string to our log applet, using the provided DBus connection.
logToDBus :: Connection -> String -> IO ()
logToDBus dbus str = do
  msg <- newSignal "/org/xmonad/Log" "org.xmonad.Log" "Update"
  addArgs msg [String tag]
  send dbus msg 0 `catchDyn` catchFun
  return ()
  where tag = sanitize $ "<span font=\"Terminus 9 Bold\">" ++ str ++ "</span>"
        catchFun (DBus.Error name msg) = return 0

-- My configured, persistent background apps.  Among other things, we have
-- a terminal, a notepad application, and a music player (okay, there are no
-- other things).
myScratchpads :: NamedScratchpads
myScratchpads = [ 
    NS "terminal" term isScratchpad nonFloating,
    NS "notepad" notepad isNotepad nonFloating,
    NS "music" music isMusicPlayer nonFloating,
    NS "torrent" torrent isTorrentClient nonFloating,
    NS "keepass" keepass isKeepass nonFloating,
    NS "android" android isAndroid nonFloating
  ]
  where term = "gnome-terminal --disable-factory --class=scratchpad --window-with-profile=Scratchpad -e 'bash -i -c \"TERM=gnome-256color tmux\"'" 
        notepad = "/home/brettk/software/wikidpad/wikidpad"
        music = "/usr/bin/exaile"
        torrent = "/usr/bin/deluge-gtk"
        keepass = "/usr/bin/keepass2"
        android = "/opt/android-sdk-linux/tools/emulator -avd Memoria -no-boot-anim -scale 0.5"

-- Here we define various special rules for how to treat windows.
myManageHook :: [ManageHook]
myManageHook = 
  [ 
    -- Properly handle fullscreen hinted windows
    isFullscreen --> doFullFloat,
    --
    -- Ignore splash windows (this includes Gnome Do) and the disk ejector thinger
    (isSplash <||> isDock <||> (className =? "Ejecter")) --> doIgnore,

    -- Normally, dialogs float, anchored at the upper-left.  But I prefer them
    -- centered.
    (isDialog <||> isFirefoxPrefs) --> doCenterFloat,

    -- Float rules for various window types
    isEvolutionChild --> dialogRectFloat,

    -- These are all scratchpad windows
    isScratchpad --> dialogRectFloat,
    isNotepad --> windowRectFloat,
    isMusicPlayer --> windowRectFloat,
    isTorrentClient --> windowRectFloat,
    isKeepass --> windowRectFloat,
    isAndroid --> androidRectFloat

    -- Otherwise, for non-dialogs, make sure we don't replace the master window
    --fmap not isDialog --> doF pushTopmostDown
  ]
  where dialogRectFloat = doRectFloat $ W.RationalRect (1/6) (1/6) (2/3) (2/3)
        windowRectFloat = doRectFloat $ W.RationalRect (1/10) (1/10) (4/5) (4/5)
        androidRectFloat = doCenterFloat

-- Here we describe our defined set of layouts.  Right now they're basically
-- the defaults, save that I name then and then use the LayoutCombinators
-- ||| operator so I can use JumpToLayout.
myLayoutHook = (named "Tall" $ tall) |||
               (named "Fat" $ wide) |||
               (named "Circle" $ circ) |||
               (named "Full" full)
  where modified = (H.HintedTile nmaster delta ratio H.TopLeft)
        tall = modified H.Tall
        wide = modified H.Wide
        circ = Circle
        full = simpleTabbed
        nmaster = 1
        ratio = 2/3
        delta = 3/100

-- Our log hook.  Here we use the logToDBus function to log to our panel applet.
myLogHook :: Connection -> X()
myLogHook dbus = (fadeOutLogHook $ fadeIf isFadeEligable fadeAmount) >> (dynamicLogWithPP logConfig)
  where fadeAmount = 0xbbbbbbbb
        logConfig = defaultPP {
          ppOutput  = logToDBus dbus,
          ppTitle   = colorize "#003366" . shorten 50,
          ppCurrent = colorize "#006666" . wrap "[" "]",
          ppVisible = colorize "#663366" . wrap "(" ")",
          ppHidden  = formatHidden,
          ppUrgent  = colorize "red"
        }
        formatHidden "NSP" = ""
        formatHidden s     = wrap " " " " s

myKeyMap :: [( String, X() )]
myKeyMap =
  [
    -- Mod-q   | Restart/reload
    ("M-q", spawn "/usr/bin/xmonad --recompile; /usr/bin/xmonad --restart"),
 
    -- Mod-g   | Add a tag to the focused window
    ("M-g", tagPrompt defaultXPConfig (\s -> withFocused (addTag s))),

    -- Mod-C-g | Delete a tag from the focused window
    ("M-S-g", tagDelPrompt defaultXPConfig),

    -- Mod-c   | Copy all windows with the given tag to the current workspace
    ("M-c", tagPrompt defaultXPConfig (\s -> withTaggedGlobalP s copyHere)),

    -- Mod-S-f | Switch to the full-screen layout 
    ("M-S-f", sendMessage $ JumpToLayout "Full"),

    -- Mod-S-t | Switch to the tall split layout
    ("M-S-t", sendMessage $ JumpToLayout "Tall"),

    -- Mod-S-w | Switch to the wide split layout
    ("M-S-w", sendMessage $ JumpToLayout "Fat"),

    -- Mod-S-c | Switch to the circle layout
    ("M-S-c", sendMessage $ JumpToLayout "Circle"),

    -- We unbind M-<Space> later so we can use Gnome Do, so we rebind 
    -- M-S-<Space> for layout cycling.
    ("M-S-<Space>", sendMessage NextLayout),

    -- Redefine the kill command (since we used M-S-c above)
    ("M-C-c", kill1),

    -- Directional navigation for windows
    -- ("M-k", sendMessage $ Go U),
    -- ("M-j", sendMessage $ Go D),
    -- ("M-h", sendMessage $ Go L),
    -- ("M-l", sendMessage $ Go R),

    -- In theory we can move windows between groups (not working yet)
    ("M-S-C-k", sendMessage $ Move U),
    ("M-S-C-j", sendMessage $ Move D),
    ("M-S-C-h", sendMessage $ Move L),
    ("M-S-C-l", sendMessage $ Move R),

    -- Since we use Vi keys, we need to rebind expand/shrink to something
    ("M-S-l", sendMessage Expand),
    ("M-S-h", sendMessage Shrink),

    -- Open up our scratchpads
    ("M-s", namedScratchpadAction myScratchpads "terminal"),
    ("M-d", namedScratchpadAction myScratchpads "notepad"),
    ("M-a", namedScratchpadAction myScratchpads "music"),
    ("M-f", namedScratchpadAction myScratchpads "torrent"),
    ("M-k", namedScratchpadAction myScratchpads "keepass"),
    ("M-l", namedScratchpadAction myScratchpads "android"),

    -- Swap these, as my primary display is usually on the left, not the
    -- right, and xmonad assumes the display ordering returned from the
    -- X server reflects the left-to-right physical arrangement of the
    -- displays.
    ("M-e", screenWorkspace 0 >>= flip whenJust (windows . W.view)),
    ("M-w", screenWorkspace 1 >>= flip whenJust (windows . W.view))
  ]

-- The set of keys we want to completely unbind (so XMonad passes them through).
myUnboundKeys :: [String]
myUnboundKeys = 
  [ 
    -- So we can use Gnome Do
    "M-<Space>",

    -- So this will pass through to rdesktop sessions
    "M-r"
  ]

-- Generate our config, using the provided DBus connection to create our logger.
myConfig dbus = gnomeConfig
  {
    terminal = "gnome-terminal --disable-factory -e 'bash -i -c \"TERM=gnome-256color tmux\"'",
    modMask = mod4Mask,
    focusFollowsMouse = False,
    borderWidth = 1,
    normalBorderColor = "#555555",
    focusedBorderColor = "#555555",
    manageHook = manageHook gnomeConfig <+> composeAll myManageHook,
    layoutHook = avoidStruts $ windowNavigation myLayoutHook,
    logHook = myLogHook dbus
  }

-- Run XMonad, using the provided DBus connection to configure our logger.
runXMonad :: Connection -> IO ()
runXMonad dbus = do
  getWellKnownName dbus
  xmonad $ ((myConfig dbus) `additionalKeysP` myKeyMap) `removeKeysP` myUnboundKeys

-- Establish a DBus connection and then fire up XMonad with that connection.
main :: IO ()
main = withConnection Session $ runXMonad

