{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses, FlexibleContexts #-}

module XMonad.MyStuff.AddRosters ( withIMs ) where

import Control.Monad (liftM, filterM)
import XMonad
import qualified XMonad.StackSet as W
import Graphics.X11.Xlib
import XMonad.Util.WindowProperties
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Grid
import XMonad.Layout.IM

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
