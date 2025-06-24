# Palindromes - Developer Notes

## Installation

To clone the repository either use an SSH key with the SHH link under the Code button.

Or to use HTTPS open a terminal in the location you want to clone to and use the command:

```
git clone <https://github.com/johanjeuring/finding-palindromes>
```

After which the repository should be cloned in your current directory.

### Set up

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

## Tooling

When contributing or writing code for this repository please install the tools listed below. This ensures your code is written in the same style as the rest of the repository.

### Hlint

For development using hlint is recommended. To install run

```
cabal install hlint
```

If in visual studio code you also need to install the haskell-linter extension.

### Style guide and autoformatting with fourmolu and cabal-fmt

Fourmolu is the autoformatter we used to automatically follow the style guide by [Kowainik (updated September 22, 2020)](https://kowainik.github.io/posts/2019-02-06-style-guide).

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

## Quality Metrics

If you want to contribute to this project we maintain the following quality metrics:

- All non-trivial code should be tested and all tests should pass.
- Your code should not degrade performance.

These quality metrics can be measured using the tools listed below.

### Testing

To test that the package works run

```
cabal test --enable-tests
```

This will run all the QuickCheck tests as well as unit tests.

### Benchmarking

To run a benchmark you need to put the files you want to benchmark into the benchmarking/benchmarking-files folder. In either the dna-files or text-files subfolder depending on the type of the file. Then run:

```
cabal bench benchmark --enable-benchmarks
```

If you have problems with this try:

```
cabal build --enable-benchmarks
```

Results of benchmarks are written into benchmark-report.html which Criterion generates to give you a complete overview.
For validating that changes did not significantly slow down the program there is a file called "benchmark-reference.html" which contains previous results of the benchmarks. Results can vary significantly per run but should not be off by more than a factor 10.

### Profiling

For profiling there is a build target in profiling/Main.hs. This main contains an example that force evaluates a linear algorithm, a quadratic algorithm and an approximate palindrome algorithm call of findPalindromes on a text file. To profile your own files/functions/settings you can replace these function calls. Additionally you can use the SCC annotations to add cost centres for your profiling.

The most basic way to run the profiling is to run the following command:

```
cabal bench profiling --enable-profiling --benchmark-options=" +RTS -p -RTS"
```

This calls the profiling executable in which you put the functions to be profiled and then creates a report in profiling.prof.
You can replace the `-p` flag with `-pj` to generate a JSON formatted report. Converting to JSON allows you to use https://www.speedscope.app/ to easily view the profiling result.

This will not give much in-depth information on the workings of the package itself however as it will only give information about the time and memory usage of functions that are exported by the modules they are in.
The best way to get information about subfunctions of the package is to create a cabal.project.local file with the following content:

```
package palindromes
  profiling: True
  ghc-options: -fprof-auto
```

Afterwards you can run the same command as before but you no longer have to use the flag `--enable-profiling`.

#### Heap Profiling

To check what is allocating memory to the heap you can pass different flags like `-hc` or `-hT` to GHC. These will generate a `.hp` report that you can use for heap profling. For information on all the flags go to: https://downloads.haskell.org/ghc/latest/docs/users_guide/profiling.html#rts-options-for-heap-profiling

First make sure that profiling is enabled using a cabal.project.local file as described in the section above, then you could for example run:

```
cabal run palindromes -- filename.txt -Q  +RTS -hc -RTS
```

One of the easiest ways to view the generated report is by using hp2pretty. You can install this using

```
cabal install hp2pretty
```

Then afterwards you can run

```
hp2pretty palindromes.hp
```

This will generate a file called palindromes.svg that you can click on to view the generated graph.

### Code coverage

The easiest way to measure code coverage is to use hpc using cabal. To do so simply run:

```
cabal test --enable-coverage
```

This will generate a few .html files that you can use to see the code coverage in different modules. hpc will tell you where these .html files were written.
We generally aim for 80% coverage for alternatives and expressions. For top level coverage we aim for a similar coverage, but this can be lowered a lot more easily by having an unused `deriving` for data types, which you can probably safely ignore.

It's good practice to generate a code coverage report before merging so you can ensure that your functions are being tested. If important parts of your code aren't covered at all you should write tests for them! If derivings of Show and Eq are only used for debugging purposes, these can probably safely be left uncovered.

## Running functions in terminal

To run and test a function in terminal use:

```
cabal repl
import [module name]
[function name] [{variables}]
```

You may need to manually use some `import` statements from the module the function is in to get the input to the right type.

For example:

```
cabal repl
import Data.Algorithms.Palindromes.Finders
import qualified Data.Vector.Unboxed as U
findPalindromeRanges VarDNA AlgQuadratic {algGapSize = 0, algMaxErrors = 0} (U.fromList "ATA")
```

This should return:

```
[(0,0),(0,2),(1,3),(3,3)]
```

## Copyright

This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.

© Copyright Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring
