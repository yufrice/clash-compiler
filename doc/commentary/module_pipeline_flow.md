## `Main` (`clash-ghc/src-ghc`)

  `clash`  binary: `src-ghc/Batch.hs`
  `clashi` binary: `src-ghc/Interactive.hs`

  1. `Clash.Main.defaultMain`

## `Clash.Main.defaultMain` (`src-bin-XYZ`)

  1. `Clash.GHC.ClashFlags.parseClashFlags`:

     Parse all the command-line `-fclash-XYZ` flags into a IORef'd
     `Clash.Driver.Types.ClashOpts`

  2. `Clash.GHCi.UI.makeHDL`

## `Clash.GHCi.UI.makeHDL` (`src-bin-XYZ`)

  1. `Clash.Backend.primDirs`

     Get all the directories containing primitive specifications corresponding
     to the current codegen backend

  2. `Clash.GHC.GenerateBindings.generateBindings`

     Load the current module and all of its dependencies and convert them to
     Clash Core.

  3. `Clash.Driver.generateHDL`

     Tranform the Clash Core to HDL code

## `Clash.GHC.GenerateBindings.generateBindings` (`src-ghc`)

  1. `Clash.GHC.LoadModules.loadModules`

     Load the current module and all of its dependencies in GHC Core form

  2. `Clash.Primitives.Util.generatePrimMap`

     Load all the primitive definitions from:

     * Locations returned from loading the modules and its dependencies, i.e.
       those returned by `InlinePrimitive`

     * Locations specified by the codegen backend (including the current working
       directory)

     * Specified as import dirs (`-i<DIR>`) on the command line

  3. `Clash.GHC.GenerateBindings.mkBindings`

     Convert the modules that are in GHC Core into Clash Core

## `Clash.GHC.LoadModules.loadModules` (`src-ghc`)

  1
