{-|
  Copyright   :  (C) 2018, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Utilities used for Clash error reporting
-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Clash.Error
  ( ClashError(..)
  , ClashValidation
  , ClashValidationException(..)
  , accumulateErrors
  , accumulateErrorsAtomic
  , clashFail
  , clashFailM
  , clashSuccess
  , clashSuccessM
  , mapAccumLMV

  , toEither
  , bindValidation
  , bindValidationM
  ) where

import           Control.Exception                 (Exception)
import           Control.Monad.State.Class         (MonadState, get, put)
import           Data.DList                        (DList)
import qualified Data.Text                         as Text
import           Data.Validation
  (Validation, toEither, fromEither, bindValidation)
import           SrcLoc                            (SrcSpan)
import           GHC.Stack
  (HasCallStack, CallStack, withFrozenCallStack, callStack)

-- | Houses a number of of @ClashError@s. Should only be used at the very top
-- of clash. For example in Driver.hs.
data ClashValidationException = ClashValidationException [ClashError]
  deriving (Show, Exception)

-- | Like @Validation@, but with a list of ClashError as its error type
type ClashValidation a = Validation (DList ClashError) a

-- | New style Clash errors. Use this type for ("probably") user errors. You
-- should probably not construct this data type manually, but use
-- @clashFail@ and @clashFailM@ instead.
--
-- ClashErrors carry a callstack (not displayed by default)
data ClashError = ClashError CallStack (Maybe SrcSpan) Text.Text
  deriving (Show)

-- Same as @Data.Validation.bindValidation@ but in a monadic context. Note that
-- despite having a bind function of the correct type, Validation is not a
-- monad. The reason is, this bind does not accumulate errors, so it does not
-- agree with the Applicative instance.
bindValidationM
  :: Monad m
  => m (Validation e a)
  -> (a -> m (Validation e b))
  -> m (Validation e b)
bindValidationM a0 f = do
  a1 <- a0
  case toEither a1 of
    Left err ->
      pure (fromEither (Left err))
    Right a2 ->
      f a2
{-# INLINE bindValidationM #-}

-- | Create a Validation.Failure with given ClashError.
clashFail
  :: HasCallStack
  => Maybe SrcSpan
  -- ^ Approximate location in user file which caused the error
  -> Text.Text
  -- ^ Error message
  -> ClashValidation a
clashFail loc msg =
  fromEither (Left (pure (ClashError callStack loc msg)))

-- | Create a Validation.Failure with given ClashError in monadic context.
clashFailM
  :: HasCallStack
  => Monad m
  => Maybe SrcSpan
  -- ^ Approximate location in user file which caused the error
  -> Text.Text
  -- ^ Error message
  -> m (ClashValidation a)
clashFailM loc msg =
  pure (withFrozenCallStack (clashFail loc msg))

-- | Create a Validation.Success with given result
clashSuccess
  :: a
  -> ClashValidation a
clashSuccess =
  fromEither . Right

-- | Create a Validation.Success with given result in monadic context
clashSuccessM
  :: Monad m
  => a
  -> m (ClashValidation a)
clashSuccessM =
  pure . clashSuccess

-- | Monadic version of 'Data.List.mapAccumL' that stops at the first error
-- it encounters.
mapAccumLMV
  :: Monad m
  => (acc -> x -> m (ClashValidation (acc, y)))
  -> acc
  -> [x]
  -> m (ClashValidation (acc, [y]))
mapAccumLMV _ acc [] =
  clashSuccessM (acc, [])
mapAccumLMV f acc0 (x:xs) =
  bindValidationM (f acc0 x) $ \(acc1, y) ->
    bindValidationM (mapAccumLMV f acc1 xs) $ \(acc2, ys) ->
      clashSuccessM (acc2, y:ys)

-- | Run a number of monadic actions that can fail using ClashValidation. Instead
-- of short-circuiting at failure, continue with the next monadic action
-- and accumulate any further errors. If you want to restore the state of the
-- state monad after a failure, see @accumulateErrorsAtomic@.
accumulateErrors
  :: HasCallStack
  => Applicative f
  => Traversable t
  => t (f (ClashValidation a))
  -> f (ClashValidation (t a))
accumulateErrors actions =
  fmap sequenceA (sequenceA actions)

-- | Run a number of monadic actions that can fail using ClashValidation. Instead
-- of short-circuiting at failure, continue with the next monadic action
-- and accumulate any further errors. At each failure, the state of the monad
-- will be restored to its state before the action was run.
accumulateErrorsAtomic
  :: HasCallStack
  => MonadState s m
  => Traversable t
  => t (m (ClashValidation a))
  -> m (ClashValidation (t a))
accumulateErrorsAtomic actions =
  accumulateErrors (mkAtomic <$> actions)
  where
    -- Make a single monadic action atomic
    mkAtomic action = do
      oldState     <- get
      actionResult <- action
      case toEither actionResult of
        Left _ -> do
          put oldState
        Right _ -> do
          return ()
      return actionResult
