{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards, TypeSynonymInstances, DeriveDataTypeable #-}

-- --------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  spencerjanssen@gmail.com
-- Stability   :  unstable
-- Portability :  not portable, Typeable deriving, mtl, posix
--
-- The collection of core layouts.
--
-----------------------------------------------------------------------------

--module XMonad.Layout (
    --Full(..), IDE(..), Mirror(..),
    --Resize(..), IncMasterN(..), Choose, (|||), ChangeLayout(..),
    --mirrorRect, splitVertically,
    --splitHorizontally, splitHorizontallyBy, splitVerticallyBy,

    --tile

  --) where
module XMonad.Layout.IDE where

import XMonad.Core
import XMonad.Layout (Resize(..), IncMasterN(..), mirrorRect)
import XMonad.Operations

import XMonad.Actions.Navigation2D

import Graphics.X11 (Rectangle(..))
import qualified XMonad.StackSet as W
import Control.Arrow ((***), second)
import Control.Monad
import Data.Maybe (fromMaybe)

------------------------------------------------------------------------
--


data CycleMaster = CycleMaster deriving (Eq, Show, Typeable)
instance Message CycleMaster

-- | The builtin tiling mode of xmonad. Supports 'Shrink', 'Expand' and
-- 'IncMasterN'.
data IDE a = IDE { tallNMaster :: !Int               -- ^ The default number of windows in the master pane (default: 1)
                   , tallRatioIncrement :: !Rational   -- ^ Percent of screen to increment by when resizing panes (default: 3/100)
                   , tallRatio :: !Rational            -- ^ Default proportion of screen occupied by master pane (default: 1/2)
                   }
                deriving (Show, Read)
                        -- TODO should be capped [0..1] ..

-- a nice pure layout, lots of properties for the layout, and its messages, in Properties.hs
instance LayoutClass IDE a where
    pureLayout (IDE nmaster _ frac) r s = zip ws rs
      where ws = W.integrate s
            rs = tile frac r nmaster (length ws)

    pureMessage (IDE nmaster delta frac) m =
            msum [fmap resize     (fromMessage m)
                 ,fmap incmastern (fromMessage m)]

      where resize Shrink             = IDE nmaster delta (max 0 $ frac-delta)
            resize Expand             = IDE nmaster delta (min 1 $ frac+delta)
            incmastern (IncMasterN d) = IDE (max 0 (nmaster+d)) delta frac

    description _ = "IDE"

-- | Compute the positions for windows using the default two-pane tiling
-- algorithm.
--
-- The screen is divided into two panes. All clients are
-- then partioned between these two panes. One pane, the master, by
-- convention has the least number of windows in it.
tile
    :: Rational  -- ^ @frac@, what proportion of the screen to devote to the master area
    -> Rectangle -- ^ @r@, the rectangle representing the screen
    -> Int       -- ^ @nmaster@, the number of windows in the master pane
    -> Int       -- ^ @n@, the total number of windows to tile
    -> [Rectangle]
tile f r nmaster n = if n <= nmaster || nmaster == 0
    then splitVertically n r
    else splitHorizontally nmaster r1 ++ splitVertically (n-nmaster) r2 -- two columns
  where (r1,r2) = splitHorizontallyBy f r

--
-- Divide the screen vertically into n subrectangles
--
splitVertically, splitHorizontally :: Int -> Rectangle -> [Rectangle]
splitVertically n r | n < 2 = [r]
splitVertically n (Rectangle sx sy sw sh) = Rectangle sx sy sw smallh :
    splitVertically (n-1) (Rectangle sx (sy+fromIntegral smallh) sw (sh-smallh))
  where smallh = sh `div` fromIntegral n --hmm, this is a fold or map.

-- Not used in the core, but exported
splitHorizontally n = map mirrorRect . splitVertically n . mirrorRect

-- Divide the screen into two rectangles, using a rational to specify the ratio
splitHorizontallyBy, splitVerticallyBy :: RealFrac r => r -> Rectangle -> (Rectangle, Rectangle)
splitHorizontallyBy f (Rectangle sx sy sw sh) =
    ( Rectangle sx sy leftw sh
    , Rectangle (sx + fromIntegral leftw) sy (sw-fromIntegral leftw) sh)
  where leftw = floor $ fromIntegral sw * f

-- Not used in the core, but exported
splitVerticallyBy f = (mirrorRect *** mirrorRect) . splitHorizontallyBy f . mirrorRect

commands :: X [(String, X ())]
commands = do
    return [ ("incMaster", sendMessage (IncMasterN 1))
           , ("swapRight", windowSwap R False)
           , ("swapLeft", windowSwap L False)
           ]
