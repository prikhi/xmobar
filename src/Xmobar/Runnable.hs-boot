{-# LANGUAGE ExistentialQuantification  #-}
module Xmobar.Runnable where
import Xmobar.Commands

data Runnable = forall r . (Exec r,Read r,Show r) => Run r

instance Read Runnable
instance Exec Runnable
