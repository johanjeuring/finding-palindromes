This is the repository for the Haskell finding Palindromes Package originally made by Johan Jeuring

# Installation

To clone the repository either use an SSH key with the SHH link under the Code button.

Or to use HTTPS open a terminal in the location you want to clone to and use the command:
```
git clone https://git.science.uu.nl/ics/sp/2025/v25b/finding-palindromes-package.git
```
This should then prompt for a login where you need to use your UU credentials (Solis ID) this should allow you to login

# Set up

If you are new to haskell use ghcup for easy installation. 
Follow instructions on the following page:
https://www.haskell.org/ghcup/

Restart. 

For installing and setting the right ghc version and tooling you can run: 
```
ghcup tui
```
Installing hls is recommended for developement. 

# Hlint

For development using hlint is recommended. To install run 
```
cabal install hlint
```
If in visual studio code you also need to install the haskell-linter extension. 

# Autoformatting with fourmolu

For Fourmolu you need a modern version of ghc. The base version needs to be atleast 4.19 so the GHC version should be atleast 9.8.2. 

Then to install fourmolu run
```
cabal install fourmolu
```
Go to system variables on device (omgevings variabelen) and add C:\cabal\bin to path.

# Running functions in terminal
To run a function in terminal use:
```
cabal repl
import [module name]
[function name] [{variables}]
```
For example:
```
cabal repl
import Data.Algorithms.Palindromes.Combinators
createPartialCombinator VarDNA ComQuadratic {gapSize = 0, maxError = 0} (0, Nothing) "ATA"
```
