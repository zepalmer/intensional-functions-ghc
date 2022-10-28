{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Compiler information functions
--
-- (c) The GHC Team 2017
--
-----------------------------------------------------------------------------
module GHC.SysTools.Info where

import GHC.Utils.Exception
import GHC.Utils.Error
import GHC.Driver.Session
import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Logger

import Data.List ( isInfixOf, isPrefixOf )
import Data.IORef

import System.IO

import GHC.Platform
import GHC.Prelude

import GHC.SysTools.Process

{- Note [Run-time linker info]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also: #5240, #6063, #10110

Before 'runLink', we need to be sure to get the relevant information
about the linker we're using at runtime to see if we need any extra
options.

Generally, the linker changing from what was detected at ./configure
time has always been possible using -pgml, but on Linux it can happen
'transparently' by installing packages like binutils-gold, which
change what /usr/bin/ld actually points to.

Clang vs GCC notes:

For gcc, 'gcc -Wl,--version' gives a bunch of output about how to
invoke the linker before the version information string. For 'clang',
the version information for 'ld' is all that's output. For this
reason, we typically need to slurp up all of the standard error output
and look through it.

Other notes:

We cache the LinkerInfo inside DynFlags, since clients may link
multiple times. The definition of LinkerInfo is there to avoid a
circular dependency.

-}

