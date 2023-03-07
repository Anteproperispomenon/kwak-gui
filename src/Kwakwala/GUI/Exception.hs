{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Kwakwala.GUI.Exception
  ( tryCont
  , tryCont'
  , tryContP
  , tryContF
  , tryContE
  , ignoreResult
  ) where

import Control.Exception

import Data.Bifunctor (first, second)
import Data.Text qualified as T
import Data.Text (Text)

-- | Try a computation, and if it fails,
-- render the exception as `Text`. Otherwise,
-- continue on with another computation.
tryCont :: forall e a b. Exception e => (IO a) -> (a -> IO b) -> IO (Either Text b)
tryCont cmp cont = do -- first displayException $ tryContE cmp cont
  eRslt <- try @e cmp
  case eRslt of
    Left  exc -> return $ Left $ T.pack $ displayException @e exc
    Right val -> Right <$> cont val


-- | Like `tryCont`, but the continuation
-- itself has a return type with `Either`.
-- This is useful when chaining together 
-- several computations that can fail.
tryCont' :: forall e a b. Exception e => (IO a) -> (a -> IO (Either Text b)) -> IO (Either Text b)
tryCont' cmp cont = do
  eRslt <- try @e cmp
  case eRslt of
    Left  exc -> return $ Left $ T.pack $ displayException @e exc
    Right val -> cont val

-- | Essentially just `(>>=)` for `Either`,
-- but where the second value is encased in `IO`.
tryContP :: forall r a b. (Either r a) -> (a -> IO (Either r b)) -> IO (Either r b)
tryContP eRslt cont = case eRslt of
  Left  x -> return $ Left x
  Right y -> cont y

-- | The final block in a `tryCont` chain. This
-- is essentially just `try`, but with the exception
-- converted to `Text`.
tryContF :: forall e a. Exception e => IO a -> IO (Either Text a)
tryContF cmp = first (T.pack . displayException) <$> try @e cmp

-- | Ignore the result of an `Either`, and only
-- pay attention to the error.
ignoreResult :: Either a b -> Maybe a
ignoreResult (Left   x) = Just x
ignoreResult (Right _y) = Nothing

-- | Try a computation, and if it fails,
-- return the exception. Otherwise,
-- continue on with another computation.
tryContE :: forall e a b. Exception e => (IO a) -> (a -> IO b) -> IO (Either e b)
tryContE cmp cont = do
  eRslt <- try @e cmp
  case eRslt of
    Left  exc -> return $ Left $ exc
    Right val -> Right <$> cont val

