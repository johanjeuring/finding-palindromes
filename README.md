# Palindromes

Palindromes is a package for finding palindromes in files.

## Features

Palindromes includes three algorithms: a linear and a quadratic worst time complexity algorithm and an algorithm for finding approximate palindromes.

All three algorithms support the following features:
- Finding plain palindromes
- Finding text palindromes,
  ignoring spaces, case of characters, and punctuation
  symbols.
- Finding palindromes in DNA.
- Finding word palindromes,
  palindromes made up of words instead of characters.

Furthermore, the individual algorithms support some other features. 
Here comes an overview of what each individual algorithm further supports. 
There is some overlap between these features, 
but we handle each algorithm separately to reduce the complexity of this text.

The linear algorithm further support:
- Finding punctuation palindromes,
  text palindromes surrounded by (if at all) non-letters.
  This requires a quadratic postprocessing step.
  However, you can still choose between the linear and 
  quadratic palindrome algorithm for finding the palindromes themselves.


The quadratic algorithm further supports:
- Finding punctuation palindromes,
  text palindromes surrounded by (if at all) non-letters.
  This requires a quadratic postprocessing step.
  However, you can still choose between the linear and 
  quadratic palindrome algorithm for finding the palindromes themselves.
- Finding approximate palindromes with substitution errors,
  in which a limited number of symbols may be substituted by other symbols to get palindromes. 
  Like 'river', which is a perfect palindrome if the 'i' is substituted for an 'e'. 
- Finding (approximate) palindromes with
  gaps in the center.

The approximate palindrome algorithm further supports:
- Finding approximate palindromes with insertion, deletion and substitution errors,
  in which a limited number of insertion, deletion or substitution operations on the 'left arm' of the approximate palindrome results in a palindrome. Like 'aaaaaaba', which is a palindrome if you add one 'b' after the first 'a'.
- Finding (approximate) palindromes with
  gaps in the center.

## Requirements

Palindromes has the following requirements:

- [GHC] version 9.4.8 or later - It has been tested with version 9.4.8 and 9.8.2
- [Cabal] library version 3.12.1 or later - It has been tested with this version

[GHC]: http://www.haskell.org/ghc/
[Cabal]: http://www.haskell.org/cabal/

## Download & Installation of the executable

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

## Usage of the executable

run the following commands, with changing `<path-to-file>` to the file containing the input, and `<options>` with the flags you want enabled
```
    cabal run palindromes -- <path-to-file> -<options>
```
Here are some examples of working flags, provided input.txt is a file in the same directory as the palindromes.cabal file:

```
    cabal run palindromes -- input.txt
    cabal run palindromes -- input.txt --quadratic --punctuation
    cabal run palindromes -- input.txt -Q
    cabal run palindromes -- input.txt -Q3+0
    cabal run palindromes -- input.txt -L --longest
    cabal run palindromes -- input.txt -A0+2 --all --details --minlength=6
    cabal run palindromes -- -i --dna
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

## Usage of the library
If you want to use the palindromes library in your code, you probably want to use the finder functions in the Finder module. 
These functions, like findPalindromes, return the found palindromes using the algorithms based on your arguments. 

If your datatype does not use the standard Eq relation for 'palindrome equality', you can define an instance of PalEq for
your datatype. For example, this is used for DNA, where A matches with T and G matches with C. 
If you do not explicitly define a PalEq instance for your datatype, but you do define an Eq instance, the Eq instance is used.   

## Documentation

The API is documented using [Haddock] and available per module on Hackage
site.

[Haddock]: http://hackage.haskell.org/package/haddock
[Palindromes package]: http://hackage.haskell.org/package/palindromes

## Licensing

Palindromes is licensed under the so-called [BSD3 license]. See the included
`LICENSE` file.

[BSD3 license]: http://www.opensource.org/licenses/bsd-license.php

## Credits

Palindromes is based on the functional program developed by [Johan Jeuring].

The current authors and maintainer of palindromes is [Johan Jeuring].

[Johan Jeuring]: http://www.jeuring.net/

## Copyright 
This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.

© Copyright Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring