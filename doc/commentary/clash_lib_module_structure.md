# $TOP

| file | comment |
| --- | --- |
| `ClashDebug.h` | Assertion and warning macros that only get enabled in a debug build |

# Clash

| module | comment |
| --- | --- |
| `Backend` | Start the Clash/GHC compiler in batch mode |
| `Driver` | Start the Clash/GHC compiler in interactive mode |
| `Netlist` | Start the Clash/GHC compiler in interactive mode |
| `Normalize` | Convert GHC Core to Clash Core |
| `Unique` | Load GHC Core definitions from `.hi` files |
| `Util` | Drive the Haskell -> Clash Core pipeline |

# Clash.Annotations.BitRepresentation

| module | comment |
| --- | --- |
| `ClashLib` | Convert Clash Core/Netlist <=> BitRepresentation |

Parts of `Clash.Annotations.BitRepresentation` originally found in the
`clash-prelude` package that cannot be located in said package as that would
create a mutual dependency between `clash-prelude` and `clash-lib`.

Deals with conversions between `clash-lib` Core and Netlist types, and
`clash-prelude` BitRepresentation types.

# Clash.Annotations.TopEntity

| module | comment |
| --- | --- |
| `Extra` | Orphan instances for `Clash.Annotations.TopEntity` needed by `clash-lib` |

# Clash.Backend

| module | comment |
| --- | --- |
| `SystemVerilog` | SystemVerilog codegen |
| `Verilog` | Verilog codegen |
| `VHDL` | VHDL codegen |

# Clash.Core

| module | comment |
| --- | --- |
| `DataCon` | Data constructors |
| `Evaluator` | Call-by-need evaluator |
| `FreeVars` | Free variable calculations |
| `Literal` | Literals |
| `Name` | Names |
| `Pretty` | Pretty printing with precedence rules for Core types |
| `Subst` | Capture-free substitution and alpha-equivalence |
| `Term` | Core terms |
| `TyCon` | Type constructors |
| `Type` | Core types |
| `TysPrim` | Hard-wired Core types and terms |
| `Util` | Random assortment of utilities for Core |
| `Var` | Variables |
| `VarEnv` | Maps with variables as keys |

# Clash.Driver

| module | comment |
| --- | --- |
| `Types` | Data types for the Clash driver pipeline |

# Clash.Netlist

| module | comment |
| --- | --- |
| `BlackBox` | Create and verify the contexts for BlackBoxes |
| `Id` | Valid Netlist identifier creation |
| `Types` | Data types for representing a netlist |
| `Util` | Random assortment of utilities for Netlist |

# Clash.Netlist.BlackBox

| module | comment |
| --- | --- |
| `Parser` | Parser for the BlackBox template language |
| `Types` | AST types for the BlackBox template language |
| `Util` | Render context into a parsed BlackBox |

# Clash.Normalize

| module | comment |
| --- | --- |
| `DEC` | Code for the DEC transformation |
| `PrimitiveReductions` | Fully unroll (recursive) primitives |
| `Strategy` | Order in which all the transformation are applied |
| `Transformations` | Transformations of the normalization process |
| `Types` | State associated with the normalization process |
| `Util` | Random assortment of utilities for Normalize |

# Clash.Primitives

| module | comment |
| --- | --- |
| `Types` | Data types and JSON parsers for Primitives |
| `Util` | Random assortment of utilities for Primitives |

# Clash.Primitives.Intel

| module | comment |
| --- | --- |
| `ClockGen` | BlackBox template functions for Intel FPGA PLLs |

# Clash.Rewrite.Combinators

| module | comment |
| --- | --- |
| `Combinators` | Combinators for transformations |
| `Types` | State associated with transformations |
| `Util` | Random assortment of utilities for rewriting |

# Clash.Util

| module | comment |
| --- | --- |
| `Graph` | Topological sorting |

# Data.Aeson

| module | comment |
| --- | --- |
| `Extra` | Utilities missing from `aeson` |

# Data.Semigroup.Monad

| module | comment |
| --- | --- |
| `Extra` | Orphan instances for `Data.Semigroup.Monad` |

# Data.Text.Prettyprint.Doc

| module | comment |
| --- | --- |
| `Extra` | `prettyprinter` combinators lifted to an `Applicative` |

# Data.Vector.Primitive

| module | comment |
| --- | --- |
| `Extra` | Orphan instances for `Data.Vector.Primitive` |

# GHC.BasicTypes

| module | comment |
| --- | --- |
| `Extra` | Orphan instances for GHC's `BasicTypes` |

# GHC.SrcLoc

| module | comment |
| --- | --- |
| `Extra` | Orphan instances for GHC's `SrcLoc` |
