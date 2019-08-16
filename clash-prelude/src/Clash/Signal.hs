{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2016-2019, Myrtle Software Ltd,
                  2017     , Google Inc.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

Clash has synchronous 'Signal's in the form of:

@
'Signal' (dom :: 'Domain') a
@

Where /a/ is the type of the value of the 'Signal', for example /Int/ or /Bool/,
and /dom/ is the /clock-/ (and /reset-/) domain to which the memory elements
manipulating these 'Signal's belong.

The type-parameter, /dom/, is of the kind 'Domain' - a simple string. That
string refers to a single /synthesis domain/. A synthesis domain describes the
behavior of certain aspects of memory elements in it. More specifically, a
domain looks like:

@
'DomainConfiguration'
  { _name :: 'Domain'
  -- ^ Domain name
  , _period :: 'Nat'
  -- ^ Clock period in /ps/
  , _edge :: 'ActiveEdge'
  -- ^ Active edge of the clock
  , _reset :: 'ResetKind'
  -- ^ Whether resets are synchronous (edge-sensitive) or asynchronous (level-sensitive)
  , _init :: 'InitBehavior'
  -- ^ Whether the initial (or "power up") value of memory elements is
  -- unknown/undefined, or configurable to a specific value
  , _polarity :: ResetPolarity
  -- ^ Whether resets are active high or active low
  }
@

Check the documentation of each of the types to see the various options Clash
provides. In order to specify a domain, an instance of 'KnownDomain' should be
made. Clash provides an implementation 'System' with some common options
chosen:

@
instance KnownDomain "System" where
  type KnownConf "System" = 'DomainConfiguration "System" 10000 'Rising 'Asynchronous 'Defined 'ActiveHigh
  knownDomain = SDomainConfiguration SSymbol SNat SRising SAsynchronous SDefined SActiveHigh
@

In words, "System" is a synthesis domain with a clock running with a period
of 10000 /ps/. Memory elements respond to the rising edge of the clock,
asynchronously to changes in their resets, and have defined power up values
if applicable.

In order to create a new domain, you don't have to instantiate it explicitly.
Instead, you can have 'createDomain' create a domain for you. You can also use
the same function to subclass existing domains.

* __NB__: \"Bad things\"™  happen when you actually use a clock period of @0@,
so do __not__ do that!
* __NB__: You should be judicious using a clock with period of @1@ as you can
never create a clock that goes any faster!
* __NB__: Whether 'System' has good defaults depends on your target platform.
Check out 'IntelSystem' and 'XilinxSystem' too!
-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_HADDOCK show-extensions #-}

#ifndef CLASH_MULTIPLE_HIDDEN
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
#endif

module Clash.Signal
  ( -- * Synchronous signals
    Signal
  , BiSignalIn
  , BiSignalOut
  , BiSignalDefault(..)
    -- * Domain
  , Domain
  , KnownDomain(..)
  , KnownConfiguration
  , ActiveEdge(..)
  , SActiveEdge(..)
  , InitBehavior(..)
  , SInitBehavior(..)
  , ResetKind(..)
  , SResetKind(..)
  , ResetPolarity(..)
  , SResetPolarity(..)
  , DomainConfiguration(..)
  , SDomainConfiguration(..)
  -- ** Configuration type families
  , DomainPeriod
  , DomainActiveEdge
  , DomainResetKind
  , DomainInitBehavior
  , DomainResetPolarity
    -- ** Default domains
  , System
  , XilinxSystem
  , IntelSystem
  , vSystem
  , vIntelSystem
  , vXilinxSystem
    -- ** Domain utilities
  , VDomainConfiguration(..)
  , vDomain
  , createDomain
  , knownVDomain
  , clockPeriod
  , activeEdge
  , resetKind
  , initBehavior
  , resetPolarity
    -- * Clock
  , Clock
  , periodToHz
  , hzToPeriod
#ifdef CLASH_MULTIPLE_HIDDEN
    -- ** Synchronization primitive
  , unsafeSynchronizer
#endif
    -- * Reset
  , Reset(..)
  , unsafeToReset
  , unsafeFromReset
  , unsafeToHighPolarity
  , unsafeToLowPolarity
  , unsafeFromHighPolarity
  , unsafeFromLowPolarity
#ifdef CLASH_MULTIPLE_HIDDEN
  , convertReset
#endif
  , E.resetSynchronizer
  , holdReset
    -- ** Enabling
  , Enable(..)
  , toEnable
  , fromEnable
  , S.enableGen
    -- * Hidden clocks and resets
    -- $hiddenclockandreset

    -- ** Hidden clock
  , HiddenClock
  , hideClock
  , exposeClock
  , withClock
#ifdef CLASH_MULTIPLE_HIDDEN
  , exposeSpecificClock
  , withSpecificClock
#endif
  , hasClock
    -- ** Hidden reset
  , HiddenReset
  , hideReset
  , exposeReset
  , exposeSpecificReset
  , withReset
  , withSpecificReset
  , hasReset
    -- ** Hidden enable
  , HiddenEnable
  , hideEnable
  , exposeEnable
  , withEnable
#ifdef CLASH_MULTIPLE_HIDDEN
  , exposeSpecificEnable
  , withSpecificEnable
#endif
  , hasEnable
    -- ** Hidden clock, reset, and enable
  , HiddenClockResetEnable
  , hideClockResetEnable
  , exposeClockResetEnable
  , withClockResetEnable
#ifdef CLASH_MULTIPLE_HIDDEN
  , exposeSpecificClockResetEnable
  , withSpecificClockResetEnable
#endif
  , SystemClockResetEnable
    -- * Basic circuit functions
  , dflipflop
  , delay
  , delayMaybe
  , delayEn
  , register
  , regMaybe
  , regEn
  , mux
    -- * Simulation and testbench functions
  , clockGen
  , resetGen
  , resetGenN
  , systemClockGen
  , systemResetGen
    -- * Boolean connectives
  , (.&&.), (.||.)
    -- * Product/Signal isomorphism
  , Bundle(..)
    -- * Simulation functions (not synthesizable)
  , simulate
  , simulateB
  , simulateN
  , simulateWithReset
  , simulateWithResetN
    -- ** lazy versions
  , simulate_lazy
  , simulateB_lazy
    -- * List \<-\> Signal conversion (not synthesizable)
  , sample
  , sampleN
  , sampleWithReset
  , sampleWithResetN
  , fromList
  , fromListWithReset
    -- ** lazy versions
  , sample_lazy
  , sampleN_lazy
  , fromList_lazy
    -- * QuickCheck combinators
  , testFor
    -- * Type classes
    -- ** 'Eq'-like
  , (.==.), (./=.)
    -- ** 'Ord'-like
  , (.<.), (.<=.), (.>=.), (.>.)
    -- * Bisignal functions
  , veryUnsafeToBiSignalIn
  , readFromBiSignal
  , writeToBiSignal
  , mergeBiSignalOuts
  )
where

import           GHC.TypeLits
  (KnownNat, KnownSymbol, AppendSymbol, Symbol, type (<=))
import           Data.Bits             (Bits) -- Haddock only
import           Data.Maybe            (isJust, fromJust)
import           Data.Proxy            (Proxy(..))
import           Prelude
import           Test.QuickCheck       (Property, property)

import           Clash.Class.HasDomain (WithSingleDomain, WithSpecificDomain)
import qualified Clash.Explicit.Signal as E
import           Clash.Explicit.Signal
  (System, resetSynchronizer, systemClockGen, systemResetGen)
import qualified Clash.Explicit.Signal as S
import           Clash.Hidden
import           Clash.Promoted.Nat    (SNat (..), snatToNum)
import           Clash.Promoted.Symbol (SSymbol (..))
import           Clash.Signal.Bundle   (Bundle (..))
import           Clash.Signal.BiSignal --(BisignalIn, BisignalOut, )
import           Clash.Signal.Internal hiding
  (sample, sample_lazy, sampleN, sampleN_lazy, simulate, simulate_lazy, testFor)
import           Clash.Signal.Internal.Ambiguous
  (knownVDomain, clockPeriod, activeEdge, resetKind, initBehavior, resetPolarity)
import qualified Clash.Signal.Internal as S
import           Clash.XException      (Undefined)

{- $setup
>>> :set -XFlexibleContexts -XTypeApplications
>>> import Clash.Promoted.Nat (SNat(..))
>>> import Clash.XException (printX)
>>> import Control.Applicative (liftA2)
>>> let oscillate = register False (not <$> oscillate)
>>> let count = regEn 0 oscillate (count + 1)
>>> :{
sometimes1 = s where
  s = register Nothing (switch <$> s)
  switch Nothing = Just 1
  switch _       = Nothing
:}

>>> :{
countSometimes = s where
  s     = regMaybe 0 (plusM (pure <$> s) sometimes1)
  plusM = liftA2 (liftA2 (+))
:}

-}

-- * Hidden clock and reset arguments

{- $hiddenclockandreset #hiddenclockandreset#
Clocks and resets are by default implicitly routed to their components. You can
see from the type of a component whether it has hidden clock or reset
arguments:

It has a hidden clock when it has a:

@
f :: 'HiddenClock' dom => ...
@

Constraint.

Or it has a hidden reset when it has a:

@
g :: 'HiddenReset' dom polarity => ...
@

Constraint.

Or it has both a hidden clock argument and a hidden reset argument when it
has a:

@
h :: 'HiddenClockReset' dom  => ..
@

Constraint.

Given a component with an explicit clock and reset arguments, you can turn them
into hidden arguments using 'hideClock' and 'hideReset'. So given a:

@
f :: Clock dom -> Reset dom -> Signal dom a -> ...
@

You hide the clock and reset arguments by:

@
-- g :: 'HiddenClockReset' dom  => Signal dom a -> ...
g = 'hideClockReset' f
@

Or, alternatively, by:

@
-- h :: HiddenClockResetEnable dom  => Signal dom a -> ...
h = f 'hasClock' 'hasReset'
@

=== Assigning explicit clock and reset arguments to hidden clocks and resets

Given a component:

@
f :: HiddenClockResetEnable dom
  => Signal dom Int
  -> Signal dom Int
@

which has hidden clock and routed reset arguments, we expose those hidden
arguments so that we can explicitly apply them:

@
-- g :: Clock dom -> Reset dom -> Signal dom Int -> Signal dom Int
g = 'exposeClockResetEnable' f
@

or, alternatively, by:

@
-- h :: Clock dom -> Reset dom -> Signal dom Int -> Signal dom Int
h clk rst = withClock clk rst f
@

Similarly, there are 'exposeClock' and 'exposeReset' to connect just expose
the hidden clock or the hidden reset argument.

You will need to explicitly apply clocks and resets when you want to use
components such as PPLs and 'resetSynchronizer':

@
topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System Int
  -> Signal System Int
topEntity clk rst =
  let (pllOut,pllStable) = 'Clash.Intel.ClockGen.altpll' (SSymbol \@\"altpll50\") clk rst
      rstSync            = 'resetSynchronizer' pllOut ('unsafeToAsyncReset' pllStable)
  in  'exposeClockResetEnable' f pllOut rstSync
@

or, using the alternative method:

@
topEntity2
  :: Clock System
  -> Reset System
  -> Signal System Int
  -> Signal System Int
topEntity2 clk rst =
  let (pllOut,pllStable) = 'Clash.Intel.ClockGen.altpll' (SSymbol \@\"altpll50\") clk rst
      rstSync            = 'resetSynchronizer' pllOut ('unsafeToAsyncReset' pllStable)
  in  'withClockReset' pllOut rstSync f
@

-}

#ifdef CLASH_MULTIPLE_HIDDEN
type HiddenClockName (dom :: Domain) = AppendSymbol dom "_clk"
type HiddenResetName (dom :: Domain) = AppendSymbol dom "_rst"
type HiddenEnableName (dom :: Domain) = AppendSymbol dom "_en"
#else
type HiddenClockName (dom :: Domain) = "clk"
type HiddenResetName (dom :: Domain) = "rst"
type HiddenEnableName (dom :: Domain) = "en"
#endif

-- | A /constraint/ that indicates the component has a hidden 'Clock'
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
type HiddenClock dom =
  ( Hidden (HiddenClockName dom) (Clock dom)
  , KnownDomain dom )

-- | A /constraint/ that indicates the component needs a 'Reset'
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
type HiddenReset dom =
  ( Hidden (HiddenResetName dom) (Reset dom)
  , KnownDomain dom )

-- | A /constraint/ that indicates the component needs a 'Enable'
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
type HiddenEnable dom =
  ( Hidden (HiddenEnableName dom) (Enable dom)
  , KnownDomain dom )

-- | A /constraint/ that indicates the component needs a 'Clock', a 'Reset',
-- and an 'Enable' belonging to the same dom.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
type HiddenClockResetEnable dom  =
  ( HiddenClock dom
  , HiddenReset dom
  , HiddenEnable dom
  )

-- | A /constraint/ that indicates the component needs a 'Clock', a 'Reset',
-- and an 'Enable' belonging to the 'System' dom.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
type SystemClockResetEnable =
  ( Hidden (HiddenClockName System) (Clock System)
  , Hidden (HiddenResetName System) (Reset System)
  , Hidden (HiddenEnableName System) (Enable System)
  )

-- | Expose a hidden 'Clock' argument of a component, so it can be applied
-- explicitly. This function can only be used on components with a single
-- domain. For example, this function will refuse when:
--
-- @
-- r ~ HiddenClock dom => Signal dom1 a -> Signal dom2 a
-- @
--
-- But will work when:
--
-- @
-- r ~ HiddenClock dom => Signal dom a -> Signal dom a
-- @
--
-- If you want to expose a clock of a component working on multiple domains
-- (such as the first example), use 'exposeSpecificClock'.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
--
-- === __Example__
-- Usage with a /polymorphic/ domain:
--
-- >>> reg = register 5 (reg + 1)
-- >>> sig = exposeClock reg clockGen
-- >>> sampleN @System 10 sig
-- [5,5,6,7,8,9,10,11,12,13]
--
-- Force exposeClock to work on System (hence 'sampleN' not needing an explicit
-- domain later):
--
-- >>> reg = register 5 (reg + 1)
-- >>> sig = exposeClock @System reg clockGen
-- >>> sampleN 10 sig
-- [5,5,6,7,8,9,10,11,12,13]
--
exposeClock
  :: forall dom  r
   . WithSingleDomain dom r
  => (HiddenClock dom => r)
  -- ^ The component with a hidden clock
  -> (KnownDomain dom => Clock dom -> r)
  -- ^ The component with its clock argument exposed
exposeClock = \f clk -> exposeSpecificClock (const f) clk (Proxy @dom)
{-# INLINE exposeClock #-}

-- | Expose a hidden 'Clock' argument of a component, so it can be applied
-- explicitly. This function can be used on components with multiple domains.
-- As opposed to 'exposeClock', callers should explicitly state what the clock
-- domain is. See the examples for more information.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
--
-- === __Example__
-- 'exposeSpecificClock' can only be used when it can find the specified domain
-- in /r/:
--
-- >>> reg = register @System 5 (reg + 1)
-- >>> sig = exposeSpecificClock @System reg clockGen
-- >>> sampleN 10 sig
-- [5,5,6,7,8,9,10,11,12,13]
--
-- Type variables work too, if they are in scope. For example:
--
-- @
-- reg = 'register' @@dom 5 (reg + 1)
-- sig = exposeSpecificClock @@dom reg 'clockGen'
-- @
--
exposeSpecificClock
   :: forall dom  r
   . WithSpecificDomain dom r
  => (HiddenClock dom => r)
  -- ^ The component with a hidden clock
  -> (KnownDomain dom => Clock dom -> r)
  -- ^ The component with its clock argument exposed
exposeSpecificClock = \f clk -> expose @(HiddenClockName dom) f clk
{-# INLINE exposeSpecificClock #-}

-- | Hide the 'Clock' argument of a component, so it can be routed implicitly.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
hideClock
  :: forall dom r
   . HiddenClock dom
  => (Clock dom -> r)
  -- ^ Function whose clock argument you want to hide
  -> r
hideClock = \f -> f (fromLabel @(HiddenClockName dom))
{-# INLINE hideClock #-}

-- | Connect an explicit 'Clock' to a function with a hidden 'Clock'. This
-- function can only be used on components with a single domain. For example,
-- this function will refuse when:
--
-- @
-- r ~ HiddenClock dom => Signal dom1 a -> Signal dom2 a
-- @
--
-- But will work when:
--
-- @
-- r ~ HiddenClock dom => Signal dom a -> Signal dom a
-- @
--
-- If you want to connect a clock to a component working on multiple domains
-- (such as the first example), use 'withSpecificClock'.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
--
-- === __Example__
-- Usage with a _polymorphic_ domain:
--
-- >>> reg = register 5 (reg + 1)
-- >>> sig = withClock clockGen reg
-- >>> sampleN @System 10 sig
-- [5,5,6,7,8,9,10,11,12,13]
--
-- Force withClock to work on signal (hence 'sampleN' not needing an explicit
-- domain later):
--
-- >>> reg = register 5 (reg + 1)
-- >>> sig = withClock @System clockGen reg
-- >>> sampleN 10 sig
-- [5,5,6,7,8,9,10,11,12,13]
--
withClock
  :: forall dom r
   . (KnownDomain dom, WithSingleDomain dom r)
  => Clock dom
  -- ^ The 'Clock' we want to connect
  -> (HiddenClock dom => r)
  -- ^ The function with a hidden 'Clock' argument
  -> r
withClock clk f = withSpecificClock clk (const f) (Proxy @dom)
{-# INLINE withClock #-}

-- | Connect an explicit 'Clock' to a function with a hidden 'Clock'. This
-- function can be used on components with multiple domains. As opposed to
-- 'exposeClock', callers should explicitly state what the clock domain is. See
-- the examples for more information.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
--
-- === __Example__
-- 'withSpecificClock' can only be used when it can find the specified domain
-- in /r/:
--
-- >>> reg = register @System 5 (reg + 1)
-- >>> sig = withClock @System clockGen reg
-- >>> sampleN 10 sig
-- [5,5,6,7,8,9,10,11,12,13]
--
-- Type variables work too, if they are in scope. For example:
--
-- @
-- reg = 'register' @@dom 5 (reg + 1)
-- sig = withClock @@dom 'clockGen' reg
-- @
--
withSpecificClock
  :: forall dom r
   . (KnownDomain dom, WithSpecificDomain dom r)
  => Clock dom
  -- ^ The 'Clock' we want to connect
  -> (HiddenClock dom => r)
  -- ^ The function with a hidden 'Clock' argument
  -> r
withSpecificClock = \clk f -> expose @(HiddenClockName dom) f clk
{-# INLINE withSpecificClock #-}

-- | Connect a hidden 'Clock' to an argument where a normal 'Clock' argument
-- was expected.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
hasClock
  :: forall dom
   . HiddenClock dom
  => Clock dom
hasClock = fromLabel @(HiddenClockName dom)
{-# INLINE hasClock #-}

-- | Expose a hidden 'Reset' argument of a component, so it can be applied
-- explicitly. This function can only be used on components with a single
-- domain. For example, this function will refuse when:
--
-- @
-- r ~ HiddenReset dom => Signal dom1 a -> Signal dom2 a
-- @
--
-- But will work when:
--
-- @
-- r ~ HiddenReset dom => Signal dom a -> Signal dom a
-- @
--
-- If you want to expose a reset of a component working on multiple domains
-- (such as the first example), use 'exposeSpecificReset'.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
--
-- === __Example__
-- Usage with a /polymorphic/ domain:
--
-- >>> reg = register 5 (reg + 1)
-- >>> sig = exposeReset reg resetGen
-- >>> sampleN @System 10 sig
-- [5,5,6,7,8,9,10,11,12,13]
--
-- Force exposeReset to work on System (hence 'sampleN' not needing an explicit
-- domain later):
--
-- >>> reg = register 5 (reg + 1)
-- >>> sig = exposeReset @System reg resetGen
-- >>> sampleN 10 sig
-- [5,5,6,7,8,9,10,11,12,13]
--
exposeReset
  :: forall dom r
   . WithSingleDomain dom r
  => (HiddenReset dom => r)
  -- ^ The component with a hidden reset
  -> (KnownDomain dom => Reset dom -> r)
  -- ^ The component with its reset argument exposed
exposeReset = \f rst -> exposeSpecificReset (const f) rst (Proxy @dom)
{-# INLINE exposeReset #-}

-- | Expose a hidden 'Reset' argument of a component, so it can be applied
-- explicitly. This function can be used on components with multiple domains.
-- As opposed to 'exposeReset', callers should explicitly state what the reset
-- domain is. See the examples for more information.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
--
-- === __Example__
-- 'exposeSpecificReset' can only be used when it can find the specified domain
-- in /r/:
--
-- >>> reg = register @System 5 (reg + 1)
-- >>> sig = exposeSpecificReset @System reg resetGen
-- >>> sampleN 10 sig
-- [5,5,6,7,8,9,10,11,12,13]
--
-- Type variables work too, if they are in scope. For example:
--
-- @
-- reg = 'register' @@dom 5 (reg + 1)
-- sig = exposeSpecificReset @@dom reg 'resetGen'
-- @
--
exposeSpecificReset
  :: forall dom r
   . WithSpecificDomain dom r
  => (HiddenReset dom => r)
  -- ^ The component with a hidden reset
  -> (KnownDomain dom => Reset dom -> r)
  -- ^ The component with its reset argument exposed
exposeSpecificReset = \f rst -> expose @(HiddenResetName dom) f rst
{-# INLINE exposeSpecificReset #-}

-- | Hide the 'Reset' argument of a component, so it can be routed implicitly.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
hideReset
  :: forall dom r
   . HiddenReset dom
  => (Reset dom -> r)
  -- ^ Component whose reset argument you want to hide
  -> r
hideReset = \f -> f (fromLabel @(HiddenResetName dom))
{-# INLINE hideReset #-}

-- | Connect an explicit 'Reset' to a function with a hidden 'Reset'. This
-- function can only be used on components with a single domain. For example,
-- this function will refuse when:
--
-- @
-- r ~ HiddenReset dom => Signal dom1 a -> Signal dom2 a
-- @
--
-- But will work when:
--
-- @
-- r ~ HiddenReset dom => Signal dom a -> Signal dom a
-- @
--
-- If you want to connect a reset to a component working on multiple domains
-- (such as the first example), use 'withSpecificReset'.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
--
-- === __Example__
-- Usage with a _polymorphic_ domain:
--
-- >>> reg = register 5 (reg + 1)
-- >>> sig = withReset resetGen reg
-- >>> sampleN @System 10 sig
-- [5,5,6,7,8,9,10,11,12,13]
--
-- Force withReset to work on signal (hence 'sampleN' not needing an explicit
-- domain later):
--
-- >>> reg = register 5 (reg + 1)
-- >>> sig = withReset @System resetGen reg
-- >>> sampleN 10 sig
-- [5,5,6,7,8,9,10,11,12,13]
--
withReset
  :: forall dom r
   . (KnownDomain dom, WithSingleDomain dom r)
  => Reset dom
  -- ^ The 'Reset' we want to connect
  -> (HiddenReset dom => r)
  -- ^ The function with a hidden 'Reset' argument
  -> r
withReset = \rst f -> expose @(HiddenResetName dom) f rst
{-# INLINE withReset #-}

-- | Connect an explicit 'Reset' to a function with a hidden 'Reset'. This
-- function can be used on components with multiple domains. As opposed to
-- 'exposeReset', callers should explicitly state what the reset domain is. See
-- the examples for more information.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
--
-- === __Example__
-- 'withSpecificReset' can only be used when it can find the specified domain
-- in /r/:
--
-- >>> reg = register @System 5 (reg + 1)
-- >>> sig = withReset @System resetGen reg
-- >>> sampleN 10 sig
-- [5,5,6,7,8,9,10,11,12,13]
--
-- Type variables work too, if they are in scope. For example:
--
-- @
-- reg = 'register' @@dom 5 (reg + 1)
-- sig = withReset @@dom 'resetGen' reg
-- @
--
withSpecificReset
  :: forall dom r
   . (KnownDomain dom, WithSpecificDomain dom r)
  => Reset dom
  -- ^ The 'Reset' we want to connect
  -> (HiddenReset dom => r)
  -- ^ The function with a hidden 'Reset' argument
  -> r
withSpecificReset = \rst f -> expose @(HiddenResetName dom) f rst
{-# INLINE withSpecificReset #-}

-- | Connect a hidden 'Reset' to an argument where a normal 'Reset' argument
-- was expected.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
hasReset
  :: forall dom
   . HiddenReset dom
  => Reset dom
hasReset = fromLabel @(HiddenResetName dom)
{-# INLINE hasReset #-}

-- | Expose a hidden 'Enable' argument of a component, so it can be applied
-- explicitly. This function can only be used on components with a single
-- domain. For example, this function will refuse when:
--
-- @
-- r ~ HiddenEnable dom => Signal dom1 a -> Signal dom2 a
-- @
--
-- But will work when:
--
-- @
-- r ~ HiddenEnable dom => Signal dom a -> Signal dom a
-- @
--
-- If you want to expose a enable of a component working on multiple domains
-- (such as the first example), use 'exposeSpecificEnable'.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
--
-- === __Example__
-- Usage with a /polymorphic/ domain:
--
-- >>> reg = register 5 (reg + 1)
-- >>> sig = exposeEnable reg enableGen
-- >>> sampleN @System 10 sig
-- [5,5,6,7,8,9,10,11,12,13]
--
-- Force exposeEnable to work on System (hence 'sampleN' not needing an explicit
-- domain later):
--
-- >>> reg = register 5 (reg + 1)
-- >>> sig = exposeEnable @System reg enableGen
-- >>> sampleN 10 sig
-- [5,5,6,7,8,9,10,11,12,13]
--
exposeEnable
  :: forall dom  r
   . WithSingleDomain dom r
  => (HiddenEnable dom => r)
  -- ^ The component with a hidden reset
  -> (KnownDomain dom => Enable dom -> r)
  -- ^ The component with its reset argument exposed
exposeEnable = \f gen -> expose @(HiddenEnableName dom) f gen
{-# INLINE exposeEnable #-}

-- | Expose a hidden 'Enable' argument of a component, so it can be applied
-- explicitly. This function can be used on components with multiple domains.
-- As opposed to 'exposeEnable', callers should explicitly state what the enable
-- domain is. See the examples for more information.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
--
-- === __Example__
-- 'exposeSpecificEnable' can only be used when it can find the specified domain
-- in /r/:
--
-- >>> reg = register @System 5 (reg + 1)
-- >>> sig = exposeSpecificEnable @System reg enableGen
-- >>> sampleN 10 sig
-- [5,5,6,7,8,9,10,11,12,13]
--
-- Type variables work too, if they are in scope. For example:
--
-- @
-- reg = 'register' @@dom 5 (reg + 1)
-- sig = exposeSpecificEnable @@dom reg 'enableGen'
-- @
--
exposeSpecificEnable
  :: forall dom r
   . WithSpecificDomain dom r
  => (HiddenEnable dom => r)
  -- ^ The component with a hidden reset
  -> (KnownDomain dom => Enable dom -> r)
  -- ^ The component with its reset argument exposed
exposeSpecificEnable = \f gen -> expose @(HiddenEnableName dom) f gen
{-# INLINE exposeSpecificEnable #-}

-- | Hide the 'Enable' argument of a component, so it can be routed implicitly.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
hideEnable
  :: forall dom r
   . HiddenEnable dom
  => (Enable dom -> r)
  -- ^ Component whose reset argument you want to hide
  -> r
hideEnable = \f -> f (fromLabel @(HiddenEnableName dom))
{-# INLINE hideEnable #-}

-- | Connect an explicit 'Enable' to a function with a hidden 'Enable'. This
-- function can only be used on components with a single domain. For example,
-- this function will refuse when:
--
-- @
-- r ~ HiddenEnable dom => Signal dom1 a -> Signal dom2 a
-- @
--
-- But will work when:
--
-- @
-- r ~ HiddenEnable dom => Signal dom a -> Signal dom a
-- @
--
-- If you want to connect a enable to a component working on multiple domains
-- (such as the first example), use 'withSpecificEnable'.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
--
-- === __Example__
-- Usage with a _polymorphic_ domain:
--
-- >>> reg = register 5 (reg + 1)
-- >>> sig = withEnable enableGen reg
-- >>> sampleN @System 10 sig
-- [5,5,6,7,8,9,10,11,12,13]
--
-- Force withEnable to work on signal (hence 'sampleN' not needing an explicit
-- domain later):
--
-- >>> reg = register 5 (reg + 1)
-- >>> sig = withEnable @System enableGen reg
-- >>> sampleN 10 sig
-- [5,5,6,7,8,9,10,11,12,13]
--
withEnable
  :: forall dom r
   . (KnownDomain dom, WithSingleDomain dom r)
  => Enable dom
  -- ^ The 'Enable' we want to connect
  -> (HiddenEnable dom => r)
  -- ^ The function with a hidden 'Enable' argument
  -> r
withEnable = \gen f -> expose @(HiddenEnableName dom) f gen
{-# INLINE withEnable #-}

-- | Connect an explicit 'Reset' to a function with a hidden 'Enable'. This
-- function can be used on components with multiple domains. As opposed to
-- 'exposeEnable', callers should explicitly state what the enable domain is. See
-- the examples for more information.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
--
-- === __Example__
-- 'withSpecificEnable' can only be used when it can find the specified domain
-- in /r/:
--
-- >>> reg = register @System 5 (reg + 1)
-- >>> sig = withEnable @System enableGen reg
-- >>> sampleN 10 sig
-- [5,5,6,7,8,9,10,11,12,13]
--
-- Type variables work too, if they are in scope. For example:
--
-- @
-- reg = 'register' @@dom 5 (reg + 1)
-- sig = withEnable @@dom 'enableGen' reg
-- @
--
withSpecificEnable
  :: forall dom r
   . (KnownDomain dom, WithSpecificDomain dom r)
  => Enable dom
  -- ^ The 'Enable' we want to connect
  -> (HiddenEnable dom => r)
  -- ^ The function with a hidden 'Enable' argument
  -> r
withSpecificEnable = \gen f -> expose @(HiddenEnableName dom) f gen
{-# INLINE withSpecificEnable #-}

-- | Connect a hidden 'Enable' to an argument where a normal 'Enable' argument
-- was expected.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
hasEnable
  :: forall dom
   . HiddenEnable dom
  => Enable dom
hasEnable = fromLabel @(HiddenEnableName dom)
{-# INLINE hasEnable #-}


-- | Expose a hidden 'Clock', 'Reset', and 'Enable' argument of a component, so
-- it can be applied explicitly. This function can only be used on components
-- with a single domain. For example, this function will refuse when:
--
-- @
-- r ~ HiddenClockResetEnable dom => Signal dom1 a -> Signal dom2 a
-- @
--
-- But will work when:
--
-- @
-- r ~ HiddenClockResetEnable dom => Signal dom a -> Signal dom a
-- @
--
-- If you want to expose a clock, reset, and enable of a component working on
-- multiple domains (such as the first example), use 'exposeSpecificClockResetEnable'.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
--
-- === __Example__
-- Usage with a /polymorphic/ domain:
--
-- >>> reg = register 5 (reg + 1)
-- >>> sig = exposeClockResetEnable reg clockGen resetGen enableGen
-- >>> sampleN @System 10 sig
-- [5,5,6,7,8,9,10,11,12,13]
--
-- Force exposeClockResetEnable to work on System (hence 'sampleN' not needing an
-- explicit domain later):
--
-- >>> reg = register 5 (reg + 1)
-- >>> sig = exposeClockResetEnable @System reg clockGen resetGen enableGen
-- >>> sampleN 10 sig
-- [5,5,6,7,8,9,10,11,12,13]
--
-- Usage in a testbench context:
--
-- @
-- topEntity :: Vec 2 (Vec 3 (Unsigned 8)) -> Vec 6 (Unsigned 8)
-- topEntity = concat
--
-- testBench :: Signal System Bool
-- testBench = done
--   where
--     testInput      = pure ((1 :> 2 :> 3 :> Nil) :> (4 :> 5 :> 6 :> Nil) :> Nil)
--     expectedOutput = outputVerifier' ((1:>2:>3:>4:>5:>6:>Nil):>Nil)
--     done           = exposeClockResetEnable (expectedOutput (topEntity <$> testInput)) clk rst
--     clk            = tbSystemClockGen (not <\$\> done)
--     rst            = systemResetGen
-- @
exposeClockResetEnable
  :: forall dom r
   . WithSingleDomain dom r
  => (HiddenClockResetEnable dom => r)
  -- ^ The component with hidden clock, reset, and enable arguments
  -> (KnownDomain dom => Clock dom -> Reset dom -> Enable dom -> r)
  -- ^ The component with its clock, reset, and enable arguments exposed
exposeClockResetEnable =
  \f clk rst en ->
    exposeSpecificClock (exposeSpecificReset (exposeEnable f)) clk rst en
{-# INLINE exposeClockResetEnable #-}

-- | Expose a hidden 'Clock', 'Reset', and 'Enable' argument of a component, so
-- it can be applied explicitly. This function can be used on components with
-- multiple domains. As opposed to 'exposeClockResetEnable', callers should
-- explicitly state what the enable domain is. See the examples for more
-- information.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
--
-- === __Example__
-- 'exposeSpecificClockResetEnable' can only be used when it can find the
-- specified domain in /r/:
--
-- >>> reg = register @System 5 (reg + 1)
-- >>> sig = exposeSpecificClockResetEnable @System reg clockGen resetGen enableGen
-- >>> sampleN 10 sig
-- [5,5,6,7,8,9,10,11,12,13]
--
-- Type variables work too, if they are in scope. For example:
--
-- @
-- reg = 'register' @@dom 5 (reg + 1)
-- sig = exposeSpecificClockResetEnable @@dom reg 'clockGen' 'resetGen' 'enableGen'
-- @
--
exposeSpecificClockResetEnable
  :: forall dom r
   . WithSpecificDomain dom r
  => (HiddenClockResetEnable dom => r)
  -- ^ The component with hidden clock, reset, and enable arguments
  -> (KnownDomain dom => Clock dom -> Reset dom -> Enable dom -> r)
  -- ^ The component with its clock, reset, and enable arguments exposed
exposeSpecificClockResetEnable =
  \f clk rst en ->
    exposeSpecificClock (exposeSpecificReset (exposeSpecificEnable f)) clk rst en
{-# INLINE exposeSpecificClockResetEnable #-}

-- -- | Hide the 'Clock' and 'Reset' arguments of a component, so they can be
-- -- routed implicitly
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
hideClockResetEnable
  :: forall dom r
   . HiddenClockResetEnable dom
  => (KnownDomain dom => Clock dom -> Reset dom -> Enable dom -> r)
  -- ^ Component whose clock, reset, and enable argument you want to hide
  -> r
hideClockResetEnable =
  \f ->
    f
      (fromLabel @(HiddenClockName dom))
      (fromLabel @(HiddenResetName dom))
      (fromLabel @(HiddenEnableName dom))
{-# INLINE hideClockResetEnable #-}

-- | Connect an explicit 'Clock', 'Reset', and 'Enable' to a function with a
-- hidden 'Clock', 'Reset', and 'Enable'. This function can only be used on
-- components with a single domain. For example, this function will refuse when:
--
-- @
-- r ~ HiddenClockResetEnable dom => Signal dom1 a -> Signal dom2 a
-- @
--
-- But will work when:
--
-- @
-- r ~ HiddenClockResetEnable dom => Signal dom a -> Signal dom a
-- @
--
-- If you want to connect a enable to a component working on multiple domains
-- (such as the first example), use 'withSpecificClockResetEnable'.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
--
-- === __Example__
-- Usage with a _polymorphic_ domain:
--
-- >>> reg = register 5 (reg + 1)
-- >>> sig = withClockResetEnable clockGen resetGen enableGen reg
-- >>> sampleN @System 10 sig
-- [5,5,6,7,8,9,10,11,12,13]
--
-- Force withClockResetEnable to work on signal (hence 'sampleN' not needing
-- an explicit domain later):
--
-- >>> reg = register 5 (reg + 1)
-- >>> sig = withClockResetEnable @System clockGen resetGen enableGen reg
-- >>> sampleN 10 sig
-- [5,5,6,7,8,9,10,11,12,13]
--
withClockResetEnable
  :: forall dom r
   . (KnownDomain dom, WithSingleDomain dom r)
  => Clock dom
  -- ^ The 'Clock' we want to connect
  -> Reset dom
  -- ^ The 'Reset' we want to connect
  -> Enable dom
  -- ^ The 'Enable' we want to connect
  -> (HiddenClockResetEnable dom => r)
  -- ^ The function with a hidden 'Clock', hidden 'Reset', and hidden
  -- 'Enable' argument
  -> r
withClockResetEnable =
  \clk rst en f -> withSpecificClockResetEnable clk rst en (const f) (Proxy @dom)
{-# INLINE withClockResetEnable #-}

-- | Connect an explicit 'Clock', 'Reset', and 'Enable' to a function with a
-- hidden 'Clock', 'Reset', and 'Enable'. This function can be used on components
-- with multiple domains. As opposed to 'exposeClockResetEnable', callers should
-- explicitly state what the enable domain is. See the examples for more
-- information.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
--
-- === __Example__
-- 'withSpecificClockResetEnable' can only be used when it can find the
-- specified domain in /r/:
--
-- >>> reg = register @System 5 (reg + 1)
-- >>> sig = withClockResetEnable @System clockGen resetGen enableGen reg
-- >>> sampleN 10 sig
-- [5,5,6,7,8,9,10,11,12,13]
--
-- Type variables work too, if they are in scope. For example:
--
-- @
-- reg = 'register' @@dom 5 (reg + 1)
-- sig = withClockResetEnable @@dom 'clockGen' 'resetGen' 'enableGen' reg
-- @
--
withSpecificClockResetEnable
  :: forall dom r
   . (KnownDomain dom, WithSpecificDomain dom r)
  => Clock dom
  -- ^ The 'Clock' we want to connect
  -> Reset dom
  -- ^ The 'Reset' we want to connect
  -> Enable dom
  -- ^ The 'Enable' we want to connect
  -> (HiddenClockResetEnable dom => r)
  -- ^ The function with a hidden 'Clock', hidden 'Reset', and hidden
  -- 'Enable' argument
  -> r
withSpecificClockResetEnable =
  \clk rst en f -> withSpecificClock clk (withSpecificReset rst (withSpecificEnable en f))
{-# INLINE withSpecificClockResetEnable #-}

-- * Basic circuit functions

-- | Special version of 'delay' that doesn't take enable signals of any kind.
-- Initial value will be undefined.
dflipflop
  :: forall dom a
   . ( HiddenClock dom
     , Undefined a )
  => Signal dom a
  -> Signal dom a
dflipflop =
  E.dflipflop (fromLabel @(HiddenClockName dom))
{-# INLINE dflipflop #-}

-- | 'delay' @s@ delays the values in 'Signal' @s@ for once cycle, the value
-- at time 0 is /dflt/.
--
-- >>> sampleN @System 3 (delay 0 (fromList [1,2,3,4]))
-- [0,1,2]
delay
  :: forall dom a
   . ( Undefined a
     , HiddenClock dom
     , HiddenEnable dom  )
  => a
  -- ^ Initial value
  -> Signal dom a
  -- ^ Signal to delay
  -> Signal dom a
delay dflt i =
  delay#
    (fromLabel @(HiddenClockName dom))
    (fromLabel @(HiddenEnableName dom))
    dflt
    i
{-# INLINE delay #-}

-- | Version of 'delay' that only updates when its second argument is a 'Just'
-- value.
--
-- >>> let input = fromList [Just 1, Just 2, Nothing, Nothing, Just 5, Just 6, Just (7::Int)]
-- >>> sampleN @System 7 (delayMaybe 0 input)
-- [0,1,2,2,2,5,6]
delayMaybe
  :: forall dom a
   . ( Undefined a
     , HiddenClock dom
     , HiddenEnable dom  )
  => a
  -- ^ Initial value
  -> Signal dom (Maybe a)
  -> Signal dom a
delayMaybe dflt i =
  E.delayMaybe
    (fromLabel @(HiddenClockName dom))
    (fromLabel @(HiddenEnableName dom))
    dflt
    i
{-# INLINE delayMaybe #-}

-- | Version of 'delay' that only updates when its second argument is asserted.
--
-- >>> let input = fromList [1,2,3,4,5,6,7::Int]
-- >>> let enable = fromList [True,True,False,False,True,True,True]
-- >>> sampleN @System 7 (delayEn 0 enable input)
-- [0,1,2,2,2,5,6]
delayEn
  :: forall dom a
   . ( Undefined a
     , HiddenClock dom
     , HiddenEnable dom  )
  => a
  -- ^ Initial value
  -> Signal dom Bool
  -- ^ Enable
  -> Signal dom a
  -> Signal dom a
delayEn dflt en i =
  E.delayEn
    (fromLabel @(HiddenClockName dom))
    (fromLabel @(HiddenEnableName dom))
    dflt
    en
    i
{-# INLINE delayEn #-}

-- | 'register' @i s@ delays the values in 'Signal' @s@ for one cycle, and sets
-- the value at time 0 to @i@
--
-- >>> sampleN @System 5 (register 8 (fromList [1,1,2,3,4]))
-- [8,8,1,2,3]
register
  :: forall dom a
   . ( HiddenClockResetEnable dom
     , Undefined a )
  => a
  -- ^ Reset value
  --
  -- 'register' outputs the reset value when the reset value is active
  -> Signal dom a
  -> Signal dom a
register i s =
  E.register
    (fromLabel @(HiddenClockName dom))
    (fromLabel @(HiddenResetName dom))
    (fromLabel @(HiddenEnableName dom))
    i
    s
{-# INLINE register #-}
infixr 3 `register`

-- | Version of 'register' that only updates its content when its second
-- argument is a 'Just' value. So given:
--
-- @
-- sometimes1 = s where
--   s = 'register' Nothing (switch '<$>' s)
--
--   switch Nothing = Just 1
--   switch _       = Nothing
--
-- countSometimes = s where
--   s     = 'regMaybe' 0 (plusM ('pure' '<$>' s) sometimes1)
--   plusM = 'liftA2' (liftA2 (+))
-- @
--
-- We get:
--
-- >>> sampleN @System 9 sometimes1
-- [Nothing,Nothing,Just 1,Nothing,Just 1,Nothing,Just 1,Nothing,Just 1]
-- >>> sampleN @System 9 countSometimes
-- [0,0,0,1,1,2,2,3,3]
regMaybe
  :: forall dom a
   . ( HiddenClockResetEnable dom
     , Undefined a )
  => a
  -- ^ Reset value. 'regMaybe' outputs the reset value when the reset is active.
  -> Signal dom (Maybe a)
  -> Signal dom a
regMaybe initial iM =
  E.regMaybe
    (fromLabel @(HiddenClockName dom))
    (fromLabel @(HiddenResetName dom))
    (fromLabel @(HiddenEnableName dom))
    initial
    iM
{-# INLINE regMaybe #-}
infixr 3 `regMaybe`

-- | Version of 'register' that only updates its content when its second argument
-- is asserted. So given:
--
-- @
-- oscillate = 'register' False ('not' '<$>' oscillate)
-- count     = 'regEn' 0 oscillate (count + 1)
-- @
--
-- We get:
--
-- >>> sampleN @System 9 oscillate
-- [False,False,True,False,True,False,True,False,True]
-- >>> sampleN @System 9 count
-- [0,0,0,1,1,2,2,3,3]
regEn
  :: forall dom a
   . ( HiddenClockResetEnable dom
     , Undefined a )
  => a
  -- ^ Reset value
  --
  -- 'regEn' outputs the reset value when the reset value is active
  -> Signal dom Bool
  -> Signal dom a
  -> Signal dom a
regEn initial en i =
  E.regEn
    (fromLabel @(HiddenClockName dom))
    (fromLabel @(HiddenResetName dom))
    (fromLabel @(HiddenEnableName dom))
    initial
    en
    i
{-# INLINE regEn #-}

-- * Signal -> List conversion

-- | Get an infinite list of samples from a 'Clash.Signal.Signal'
--
-- The elements in the list correspond to the values of the 'Signal'
-- at consecutive clock cycles
--
-- > sample s == [s0, s1, s2, s3, ...
--
-- If the given component has not yet been given a clock, reset, or enable
-- line, 'sample' will supply them. The reset will be asserted for a single
-- cycle. 'sample' will not drop the value produced by the circuit while
-- the reset was asserted. If you want this, or if you want more than a
-- single cycle reset, consider using 'sampleWithReset'.
--
-- __NB__: This function is not synthesizable
sample
  :: forall dom a
   . ( KnownDomain dom
     , Undefined a )
  => (HiddenClockResetEnable dom  => Signal dom a)
  -- ^ 'Signal' we want to sample, whose source potentially has a hidden clock
  -- (and reset)
  -> [a]
sample s =
  S.sample (exposeClockResetEnable s clockGen resetGen enableGen)
{-# NOINLINE sample #-}

-- | Get a list of /n/ samples from a 'Signal'
--
-- The elements in the list correspond to the values of the 'Signal'
-- at consecutive clock cycles
--
-- > sampleN @System 3 s == [s0, s1, s2]
--
-- If the given component has not yet been given a clock, reset, or enable
-- line, 'sampleN' will supply them. The reset will be asserted for a single
-- cycle. 'sampleN' will not drop the value produced by the circuit while
-- the reset was asserted. If you want this, or if you want more than a
-- single cycle reset, consider using 'sampleWithResetN'.
--
-- __NB__: This function is not synthesizable
sampleN
  :: forall dom a
   . ( KnownDomain dom
     , Undefined a )
  => Int
  -- ^ Number of samples to produce
  -> (HiddenClockResetEnable dom => Signal dom a)
  -- ^ 'Signal' to sample, whose source potentially has a hidden clock
  -- (and reset)
  -> [a]
sampleN n s0 =
  let s1 = exposeClockResetEnable s0 clockGen resetGen enableGen in
  S.sampleN n s1
{-# NOINLINE sampleN #-}

-- | Get a list of samples from a 'Signal', while asserting the reset line
-- for /n/ clock cycles. 'sampleWithReset' does not return the first /n/ cycles,
-- i.e., when the reset is asserted.
--
-- __NB__: This function is not synthesizable
sampleWithReset
  :: forall dom a m
   . ( KnownDomain dom
     , Undefined a
     , 1 <= m )
  => SNat m
  -- ^ Number of cycles to assert the reset
  -> (HiddenClockResetEnable dom => Signal dom a)
  -- ^ 'Signal' to sample, whose source potentially has a hidden clock
  -- (and reset)
  -> [a]
sampleWithReset nReset f0 =
  let f1 = exposeClockResetEnable f0 clockGen (resetGenN @dom nReset) enableGen in
  drop (snatToNum nReset) (S.sample f1)
{-# NOINLINE sampleWithReset #-}

-- | Get a fine list of /m/ samples from a 'Signal', while asserting the reset line
-- for /n/ clock cycles. 'sampleWithReset' does not return the first /n/ cycles,
-- i.e., while the reset is asserted.
--
-- __NB__: This function is not synthesizable
sampleWithResetN
  :: forall dom a m
   . ( KnownDomain dom
     , Undefined a
     , 1 <= m )
  => SNat m
  -- ^ Number of cycles to assert the reset
  -> Int
  -- ^ Number of samples to produce
  -> (HiddenClockResetEnable dom => Signal dom a)
  -- ^ 'Signal' to sample, whose source potentially has a hidden clock
  -- (and reset)
  -> [a]
sampleWithResetN nReset nSamples f =
  take nSamples (sampleWithReset nReset f)

-- | /Lazily/ get an infinite list of samples from a 'Clash.Signal.Signal'
--
-- The elements in the list correspond to the values of the 'Signal'
-- at consecutive clock cycles
--
-- > sample s == [s0, s1, s2, s3, ...
--
-- If the given component has not yet been given a clock, reset, or enable
-- line, 'sample_lazy' will supply them. The reset will be asserted for a
-- single cycle. 'sample_lazy' will not drop the value produced by the
-- circuit while the reset was asserted.
--
-- __NB__: This function is not synthesizable
sample_lazy
  :: forall dom a
   . KnownDomain dom
  => (HiddenClockResetEnable dom  => Signal dom a)
  -- ^ 'Signal' we want to sample, whose source potentially has a hidden clock
  -- (and reset)
  -> [a]
sample_lazy s =
  S.sample_lazy (exposeClockResetEnable s clockGen resetGen enableGen)
{-# NOINLINE sample_lazy #-}

-- | Lazily get a list of /n/ samples from a 'Signal'
--
-- The elements in the list correspond to the values of the 'Signal'
-- at consecutive clock cycles
--
-- > sampleN @System 3 s == [s0, s1, s2]
--
-- If the given component has not yet been given a clock, reset, or enable
-- line, 'sampleN_lazy' will supply them. The reset will be asserted for a
-- single cycle. 'sampleN_lazy' will not drop the value produced by the
-- circuit while the reset was asserted.
--
-- __NB__: This function is not synthesizable
sampleN_lazy
  :: forall dom a
   . KnownDomain dom
  => Int
  -> (HiddenClockResetEnable dom  => Signal dom a)
  -- ^ 'Signal' we want to sample, whose source potentially has a hidden clock
  -- (and reset)
  -> [a]
sampleN_lazy n s =
  S.sampleN_lazy n (exposeClockResetEnable s clockGen resetGen enableGen)
{-# NOINLINE sampleN_lazy #-}

-- * Simulation functions

-- | Simulate a (@'Signal' a -> 'Signal' b@) function given a list of samples
-- of type /a/
--
-- >>> simulate @System (register 8) [1, 2, 3]
-- [8,1,2,3...
-- ...
--
-- Where 'System' denotes the /domain/ to simulate on. The reset line is
-- asserted for a single cycle. The first value is therefore supplied twice to
-- the circuit: once while reset is high, and once directly after. The first
-- /output/ value (the value produced while the reset is asserted) is dropped.
--
-- If you only want to simulate a finite number of samples, see 'simulateN'. If
-- you need the reset line to be asserted for more than one cycle or if you
-- need a custom reset value, see 'simulateWithReset' and 'simulateWithResetN'.
--
-- __NB__: This function is not synthesizable
simulate
  :: forall dom a b
   . ( KnownDomain dom
     , Undefined a
     , Undefined b )
  => (HiddenClockResetEnable dom => Signal dom a -> Signal dom b)
  -- ^ Circuit to simulate, whose source potentially has a hidden clock, reset,
  -- and/or enable.
  -> [a]
  -> [b]
simulate f as = simulateWithReset (SNat @1) (head as) f as
{-# INLINE simulate #-}

-- | Same as 'simulate', but only sample the first /Int/ output values.
--
-- __NB__: This function is not synthesizable
simulateN
  :: forall dom a b
   . ( KnownDomain dom
     , Undefined a
     , Undefined b )
  => Int
  -- ^ Number of cycles to simulate (excluding cycle spent in reset)
  -> (HiddenClockResetEnable dom => Signal dom a -> Signal dom b)
  -- ^ 'Signal' we want to sample, whose source potentially has a hidden clock
  -- (and reset)
  -> [a]
  -> [b]
simulateN n f as = simulateWithResetN (SNat @1) (head as) n f as
{-# INLINE simulateN #-}

-- | Same as 'simulate', but with the reset line asserted for /n/ cycles. Similar
-- to 'simulate', 'simulateWithReset' will drop the output values produced while
-- the reset is asserted. While the reset is asserted, the reset value /a/ is
-- supplied to the circuit.
simulateWithReset
  :: forall dom a b m
   . ( KnownDomain dom
     , Undefined a
     , Undefined b
     , 1 <= m )
  => SNat m
  -- ^ Number of cycles to assert the reset
  -> a
  -- ^ Reset value
  -> (HiddenClockResetEnable dom => Signal dom a -> Signal dom b)
  -- ^ 'Signal' we want to sample, whose source potentially has a hidden clock
  -- (and reset)
  -> [a]
  -> [b]
simulateWithReset n resetVal f as =
  S.simulateWithReset n resetVal (exposeClockResetEnable f) as
{-# INLINE simulateWithReset #-}

-- | Same as 'simulateWithReset', but only sample the first /Int/ output values.
simulateWithResetN
  :: forall dom a b m
   . ( KnownDomain dom
     , Undefined a
     , Undefined b
     , 1 <= m )
  => SNat m
  -- ^ Number of cycles to assert the reset
  -> a
  -- ^ Reset value
  -> Int
  -- ^ Number of cycles to simulate (excluding cycles spent in reset)
  -> (HiddenClockResetEnable dom => Signal dom a -> Signal dom b)
  -- ^ 'Signal' we want to sample, whose source potentially has a hidden clock
  -- (and reset)
  -> [a]
  -> [b]
simulateWithResetN nReset resetVal nSamples f as =
  S.simulateWithResetN nReset resetVal nSamples (exposeClockResetEnable f) as
{-# INLINE simulateWithResetN #-}


-- | /Lazily/ simulate a (@'Signal' a -> 'Signal' b@) function given a list of
-- samples of type /a/
--
-- >>> simulate @System (register 8) [1, 2, 3]
-- [8,1,2,3...
-- ...
--
-- __NB__: This function is not synthesizable
simulate_lazy
  :: forall dom a b
   . KnownDomain dom
  => (HiddenClockResetEnable dom  =>
      Signal dom a -> Signal dom b)
  -- ^ Function we want to simulate, whose components potentially have a hidden
  -- clock (and reset)
  -> [a]
  -> [b]
simulate_lazy f0 =
  let f1 = exposeClockResetEnable f0 clockGen resetGen enableGen in
  tail . S.simulate_lazy f1 . dup1
{-# NOINLINE simulate_lazy #-}

-- | Simulate a (@'Unbundled' a -> 'Unbundled' b@) function given a list of
-- samples of type @a@
--
-- >>> simulateB @System (unbundle . register (8,8) . bundle) [(1,1), (2,2), (3,3)] :: [(Int,Int)]
-- [(8,8),(1,1),(2,2),(3,3)...
-- ...
--
-- __NB__: This function is not synthesizable
simulateB
  :: forall dom a b
   . ( KnownDomain dom
     , Bundle a
     , Bundle b
     , Undefined a
     , Undefined b
     )
  => (HiddenClockResetEnable dom  =>
      Unbundled dom a -> Unbundled dom b)
  -- ^ Function we want to simulate, whose components potentially have a hidden
  -- clock (and reset)
  -> [a]
  -> [b]
simulateB f0 =
  tail . S.simulateB f1 . dup1
 where
  f1 =
    withSpecificClockResetEnable
      @dom
      clockGen
      resetGen
      enableGen
      (const f0)
      (Proxy @dom)
{-# NOINLINE simulateB #-}

-- | /Lazily/ simulate a (@'Unbundled' a -> 'Unbundled' b@) function given a
-- list of samples of type @a@
--
-- >>> simulateB @System (unbundle . register (8,8) . bundle) [(1,1), (2,2), (3,3)] :: [(Int,Int)]
-- [(8,8),(1,1),(2,2),(3,3)...
-- ...
--
-- __NB__: This function is not synthesizable
simulateB_lazy
  :: forall dom a b
   . ( KnownDomain dom
     , Bundle a
     , Bundle b )
  => (HiddenClockResetEnable dom  =>
      Unbundled dom a -> Unbundled dom b)
  -- ^ Function we want to simulate, whose components potentially have a hidden
  -- clock (and reset)
  -> [a]
  -> [b]
simulateB_lazy f0 =
  tail . S.simulateB_lazy f1 . dup1
 where
  f1 =
    withSpecificClockResetEnable
      @dom
      clockGen
      resetGen
      enableGen
      (const f0)
      (Proxy @dom)
{-# NOINLINE simulateB_lazy #-}

dup1 :: [a] -> [a]
dup1 (x:xs) = x:x:xs
dup1 _      = error "empty list"

-- * QuickCheck combinators

-- |  @testFor n s@ tests the signal /s/ for /n/ cycles.
testFor
  :: KnownDomain dom
  => Int
  -- ^ The number of cycles we want to test for
  -> (HiddenClockResetEnable dom  => Signal dom Bool)
  -- ^ 'Signal' we want to evaluate, whose source potentially has a hidden clock
  -- (and reset)
  -> Property
testFor n s = property (and (Clash.Signal.sampleN n s))

#ifdef CLASH_MULTIPLE_HIDDEN
-- ** Synchronization primitive
-- | Implicit version of 'Clash.Explicit.Signal.unsafeSynchronizer'.
unsafeSynchronizer
  :: forall dom1 dom2 a
   . ( HiddenClock dom1
     , HiddenClock dom2 )
  => Signal dom1 a
  -> Signal dom2 a
unsafeSynchronizer =
  hideClock (hideClock S.unsafeSynchronizer)
#endif

-- | Hold reset for a number of cycles relative to an implicit reset signal.
--
-- Example:
--
-- >>> sampleN @System 8 (unsafeToHighPolarity (holdReset (SNat @2)))
-- [True,True,True,False,False,False,False,False]
--
-- 'holdReset' holds the reset for an additional 2 clock cycles for a total
-- of 3 clock cycles where the reset is asserted.
--
holdReset
  :: forall dom m
   . HiddenClockResetEnable dom
  => SNat m
  -- ^ Hold for /m/ cycles, counting from the moment the incoming reset
  -- signal becomes deasserted.
  -> Reset dom
holdReset m =
  hideClockResetEnable (\clk rst en -> E.holdReset clk en m rst)

-- | Like 'fromList', but resets on reset and has a defined reset value.
--
-- >>> let rst = unsafeFromHighPolarity (fromList [True, True, False, False, True, False])
-- >>> let res = withReset rst (fromListWithReset Nothing [Just 'a', Just 'b', Just 'c'])
-- >>> sampleN @System 6 res
-- [Nothing,Nothing,Just 'a',Just 'b',Nothing,Just 'a']
--
-- __NB__: This function is not synthesizable
fromListWithReset
  :: forall dom a
   . (HiddenReset dom, Undefined a)
  => a
  -> [a]
  -> Signal dom a
fromListWithReset = hideReset E.fromListWithReset
{-# INLINE fromListWithReset #-}

#ifdef CLASH_MULTIPLE_HIDDEN
-- | Convert between different types of reset, adding a synchronizer in case
-- it needs to convert from an asynchronous to a synchronous reset.
convertReset
  :: forall domA domB
   . ( HiddenClock domA
     , HiddenClock domB
     )
  => Reset domA
  -> Reset domB
convertReset =
  E.convertReset hasClock hasClock
#endif
