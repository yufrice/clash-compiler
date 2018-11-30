# source tree roadmap

#### $TOP

| file | comment |
| --- | --- |
| `.gitlib-ci.yml` | Gitlab CI/CD configuration |
| `.travis.yml` | TravisCI configuration |
| `Clash.hs` | Setup clash pipeline for running clash in an interactive session |
| `cabal.project` | Cabal configuration |
| `clash-dev` | Load simple Clash driver pipeline into an interactive GHC session |

#### .ci

Docker, build, and tests scripts for all CIs

#### .circleci

CircleCI configuration

#### benchmark

Benchmarking suite

#### clash-cosim

Call Verilog directly from Haskell

#### clash-ghc

Clash GHC frontend

##### cbits

Some C-dingbats used by the GHC compiler binary

##### src-bin-XYZ

Nearly verbatim copy of version XYZ of the GHC compiler binary source code.
Slightly modified to:

* Enable certain language flags by default
* Load type numeric type-checker plugins by default
* Support `--HDL` batch modes, and `:HDL` commands in the interactive sessions.

##### src-ghc

Application logic for the `--HDL` batch modes and `:HDL` commands.
Interacts with the GHC API to transform Haskell to GHC's Core, and loads
external definitions from `.hi` files; then transforms them to Clash' Core.

#### clash-lib

Core Clash compiler library

#### clash-prelude

Circuit base library

#### doc

Compiler documentation and commentary

#### examples

Small example circuits

#### tests

Regression tests

#### testsuite

Regression test driver
