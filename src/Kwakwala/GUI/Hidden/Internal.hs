{-# LANGUAGE TemplateHaskell #-}

module Kwakwala.GUI.Hidden.Internal
  ( HiddenVal(..)
  , hiddenVal
  ) where

-- For when you want to use a value internally,
-- but don't want to expose it when working 
-- with it.

import Control.Lens
import Data.Default

newtype HiddenVal a
  = HiddenVal {_hiddenVal :: a}
  deriving (Show, Eq)

makeLenses 'HiddenVal

instance (Default a) => Default (HiddenVal a) where
  def = HiddenVal def