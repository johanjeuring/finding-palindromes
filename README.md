# Palindromes

Palindromes is a package for finding palindromes in files.

## Features

Palindromes includes two algorithms: a linear and a quadratic worst time complexity algorithm.

The linear and quadratic algorithm support:

- Finding plain palindromes
- Finding text palindromes,
  ignoring spaces, case of characters, and punctuation
  symbols.
- Finding palindromes in DNA.
- Finding word palindromes,
  palindromes made up of words instead of characters.

Furthermore, the quadratic algorithm supports:

- Finding punctuation palindromes,
  text palindromes surrounded by (if at all) non-letters.
  This is always quadratic because it requires a quadratic postprocessing step.
  However, you can still choose between a quadratic or linear algorithm for finding the palindromes themselves.
- Finding approximate palindromes,
  in which a limited number of symbols
  may be substituted by other symbols.
- Finding palindromes with
  gaps in the center.

## Requirements

Palindromes has the following requirements:

- [GHC] version 9.4.8 or later - It has been tested with version 9.4.8 and 9.8.2
- [Cabal] library version 3.12.1 or later - It has been tested with this version

[GHC]: http://www.haskell.org/ghc/
[Cabal]: http://www.haskell.org/cabal/

## Download & Installation

_If you have [cabal-install]_, you should use that to install the package,
because it will handle everything for you.

    cabal install exe:palindromes

_If you don't have cabal-install_, you must download the [Palindromes package]
from HackageDB and install it manually. Get the `tar.gz` file and decompress it.

Once downloaded, use the following commands for configuring, building, and
installing the library.

    runghc Setup.lhs configure
    runghc Setup.lhs build
    runghc Setup.lhs install

For more details on the general options available, refer to the [Cabal User's
Guide].

[Palindromes package]: http://hackage.haskell.org/package/palindromes
[cabal-install]: http://www.haskell.org/haskellwiki/Cabal-Install
[Cabal User's Guide]: http://www.haskell.org/cabal/users-guide/

## Usage

run the following commands, with changing `<path-to-file>` to the file containing the input, and `<options>` with the flags you want enabled

    cabal run palindromes -- <path-to-file> -<options>

Here are some examples of working flags:

```
    cabal run palindromes -- input.txt
    cabal run palindromes -- input.txt -Q -n
    cabal run palindromes -- input.txt -L -d
    cabal run palindromes -- input.txt -Q
    cabal run palindromes -- input.txt -Q3+0
```

To see all the options run one of these:

```
    cabal run palindromes
    cabal run palindromes -- -h
    cabal run palindromes -- --help
```

If you want to see unicode characters in the output in a PowerShell terminal on Windows, try to use the following commands in the terminal:
```
    $OutputEncoding = [Console]::OutputEncoding = [Text.UTF8Encoding]::UTF8
```

## Documentation

The API is documented using [Haddock] and available per module on Hackage
site.

[Haddock]: http://hackage.haskell.org/package/haddock
[Palindromes package]: http://hackage.haskell.org/package/palindromes

## Examples

You can find example palindromes, on which Palindromes has been tested, in the
[`examples` directory] of the source distribution.

[`examples` directory]: PLACEHOLDER

## Bugs & Support

To report bugs, use the [GitLab issue page].
[GitLab issue page]: PLACEHOLDER

## Licensing

Palindromes is licensed under the so-called [BSD3 license]. See the included
`LICENSE` file.

[BSD3 license]: http://www.opensource.org/licenses/bsd-license.php

## Credits

Palindromes is based on the functional program developed by [Johan Jeuring] in
his PhD thesis.

The current authors and maintainer of palindromes is [Johan Jeuring].

[Johan Jeuring]: http://www.jeuring.net/
