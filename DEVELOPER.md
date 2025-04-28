# Developer Notes

# Contributing

When contributing or writing code for this repository please install the tools listed below. This ensures your code is written in the same style as the rest of the repository.

# Installation

To clone the repository either use an SSH key with the SHH link under the Code button.

Or to use HTTPS open a terminal in the location you want to clone to and use the command:

```
git clone https://git.science.uu.nl/ics/sp/2025/v25b/finding-palindromes-package.git
```

This should then prompt for a login where you need to use your UU credentials (Solis ID). After which the repository should be cloned

## Set up

If you are new to Haskell use ghcup for easy installation.
Follow instructions on the following page:
https://www.haskell.org/ghcup/

Restart.

For installing and setting the right ghc version and tooling you can run:

```
ghcup tui
```

To use the correct version of Fourmolu we recommend you install ghc version 9.8.2

Installing hls is recommended for developement.

## Hlint

For development using hlint is recommended. To install run

```
cabal install hlint
```

If in visual studio code you also need to install the haskell-linter extension.

## Autoformatting with fourmolu and cabal-fmt

To properly run fourmolu you need 4 things.

- Atleast GHC 9.8.2
- Fourmolu version 0.18.0
- The VSCode extension Run on Save
- C:\Cabal\bin should be part of your system path

For fourmolu 0.18.0 the base version needs to be atleast 4.19 so the GHC version should be atleast 9.8.2.

Then to install fourmolu run

```
cabal install fourmolu
```

For formatting the cabal file we use the cabal-fmt formatter.
You should download the VsCode extension cabal-fmt and then also run the command:

```
cabal install cabal-fmt
```

For Windows type in system variables into the search bar and add C:\cabal\bin to the PATH variable. This ensures that you can access your cabal executables anywhere in your system.

## Testing

To test that the package works run

```
cabal test
```

This will run all the quickCheck properties as well as unit tests.

## Benchmarking

To run a benchmark you need to put the files you want to benchmark into the benchmarking-files folder. In either the Dna or Text subfolder depending on the type of the file. Then run:

```
cabal bench benchmark
```

If you have problems with this try either:

```
cabal bench benchmark --enable-benchmarking
```

or

```
cabal build --enable-benchmarking
```

Results of benchmarks are written into benchmark-report.html which Criterion generates to give you a complete overview.
For validating that changes did not significantly slow down the program there is a file called "benchmark-reference.html" which contains previous results of the benchmarks. Results can vary significantly per run but should not be off by more than a factor 10.

## Running functions in terminal

To run and test a function in terminal use:

```
cabal repl
import [module name]
[function name] [{variables}]
```

For example:

```
cabal repl
import Data.Algorithms.Palindromes.Finders
findPalindromeLengths VarDNA ComQuadratic {gapSize = 0, maxError = 0} (0, Nothing) "ATA"
```
