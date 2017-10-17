-- paste from https://stackoverflow.com/questions/6605399/how-can-i-set-an-action-to-occur-on-a-key-release-in-xmonad#11308086
module KeyDown where
    
import Data.Monoid
import qualified Data.Map as M

import XMonad
import Control.Monad
import Graphics.X11.ExtraTypes.XF86

keyDownEventHook :: Event -> X All
keyDownEventHook e = handle e >> return (All True)

keyDownKeys (XConf{ config = XConfig {XMonad.modMask = modMask} }) = M.fromList $ 
    [
    ((modMask, xK_a), spawn("xmessage coucou"))
    , ((0,                 xF86XK_Mail), spawn "easymove.sh")
    ]

handle :: Event -> X ()
handle (KeyEvent {ev_event_type = t, ev_state = m, ev_keycode = code})
    | t == keyRelease = withDisplay $ \dpy -> do
        s  <- io $ keycodeToKeysym dpy code 0
        mClean <- cleanMask m
        ks <- asks keyDownKeys
        userCodeDef () $ whenJust (M.lookup (mClean, s) ks) id
handle _ = return ()
