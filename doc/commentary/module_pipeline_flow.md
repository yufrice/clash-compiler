# `Main` (`clash-ghc/src-ghc`)

  `clash`  binary: `src-ghc/Batch.hs`
  `clashi` binary: `src-ghc/Interactive.hs`

  1. `Clash.Main.defaultMain`

## `Clash.Main.defaultMain` (`src-bin-XYZ`)

  1. `Clash.GHC.ClashFlags.parseClashFlags`:

     Parse all the command-line `-fclash-XYZ` flags into a IORef'd
     `Clash.Driver.Types.ClashOpts`

  2. `Clash.GHCi.UI.makeHDL`

### `Clash.GHCi.UI.makeHDL` (`src-bin-XYZ`)

  1. `Clash.Backend.primDirs`

     Get all the directories containing primitive specifications corresponding
     to the current codegen backend

  2. `Clash.GHC.GenerateBindings.generateBindings`

     Load the current module and all of its dependencies and convert them to
     Clash Core.

  3. `Clash.Driver.generateHDL`

     Tranform the Clash Core to HDL code

#### `Clash.GHC.GenerateBindings.generateBindings` (`src-ghc`)

  1. `Clash.GHC.LoadModules.loadModules`

     Load the current module and all of its dependencies in GHC Core form

  2. `Clash.Primitives.Util.generatePrimMap`

     Load all the primitive definitions from:

     * Locations returned from loading the modules and its dependencies, i.e.
       those returned by `InlinePrimitive`

     * Locations specified by the codegen backend (including the current working
       directory)

     * Specified as import dirs (`-i<DIR>`) on the command line

  3. `Clash.GHC.GHC2Core.coreToTerm`

     Convert the modules that are in GHC Core into Clash Core

##### `Clash.GHC.LoadModules.loadModules` (`src-ghc`)

  1. Parses, typechecks, simplifies/optimises, and then tidies the module into
     a set of GHC Core binders

  2. `Clash.GHC.LoadInterfaceFiles.loadExternalExprs` (`src-ghc`)

     Load GHC Core binders from other packages by reading them from the `.hi`
     files

  3. `Clash.GHC.LoadModules.findXYZAnnotations` (`src-ghc`)

     Load all the different Clash annotations (`Synthesize` and friends) from
     the set of binders in the current module set, i.e. not from external
     packages.

     (TODO: this seems wrong, we should be loading Clash annotations from the
     externally defined binding as well)

  4. Return all the binders

###### `Clash.GHC.LoadInterfaceFiles.loadExternalExprs` (`src-ghc`)

  1. Start from a set of visited binders

  2. For every visited binder, collect its free variables (`CoreFVs.exprSomeFreeVarsList`)

  3. `Clash.GHC.LoadInterfaceFiles.loadExprFromIface`
     Try to load the term corresponding to the free variable, the external
     dependency, from the an `.hi` interface file

  4. Make the transitive closure

#### `Clash.Driver.generateHDL` (`clash-lib/src`)

  For every topEntity:

  1. Calculate a hash of the topEntity and its transitive dependencies to
     determine whether we need to recompile

  2. `Clash.Driver.normalizeEntity`

     Transform the topEntity and its transitive dependencies to the "desired",
     normalized, form from which conversion to netlist is trivial

  3. `Clash.Netlist.genNetlist`

     Convert the normalized Clash Core bindings into the Netlist data types

  4. `Clash.Driver.createHDL`

     Pretty print the netlist to the desired HDL

  5. Write the HDL to file, copy the collected data files, and write memory
     files.

##### `Clash.Driver.normalizeEntity`

  1. `Clash.Normalize.normalize`

     Transform the topEntity and its transitive dependencies to the "desired",
     normalized, form from which conversion to netlist is trivial

  2. `Clash.Normalize.cleanupGraph`

     After checking that the graph of binders coming out of the normalization
     process is not recursive perform some inlining so as not to end up with
     100s of HDL files.

###### `Clash.Normalize.normalize`

  1. `Clash.Normalize.rewriteExpr`

     Rewrite the expression using the `Clash.Normalize.Strategy.normalization`
     strategy

  2. Collect the free variables (`Clash.Core.FreeFars.termFreeIds`) of the
     normalized binding, and normalize them.

  3. Make the transitive closure of the above process

####### `Clash.Normalize.Strategy.normalization`

  * Chapter 4 of:
    Baaij, C.P.R. (2015) Digital Circuits in CλaSH: Functional Specifications
    and Type-Directed Synthesis. PhD thesis, University of Twente, Enschede,
    The Netherlands, January 2015.

  * Without all the cast propagation transformations

  * Plus optimisations such as DEC

##### `Clash.Netlist.genNetlist`

##### `Clash.Driver.createHDL`
