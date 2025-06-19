# Palindromes

Palindromes is a package for finding palindromes in files.

## Features

Palindromes includes three algorithms: a linear complexity algorithm, a quadratic time complexity algorithm and another quadratic time algorithm for finding approximate palindromes.

All three algorithms support the following features:
- Finding plain palindromes
- Finding text palindromes,
  ignoring spaces, case of characters, and punctuation
  symbols.
- Finding palindromes in DNA.
- Finding word palindromes,
  palindromes made up of words instead of characters.

Now follows an overview of features the individual algorithms support. 
There is some overlap between these features, these are mentioned twice. 
We also mention advantages and disadvantages of the algorithms.

The linear algorithm further support:
- Finding punctuation palindromes,
  text palindromes surrounded by (if at all) non-letters.
  This requires a quadratic postprocessing step.
- When there is many very large palindromes in the text, this algorithm is the fastest.  

The quadratic algorithm further supports:
- Finding punctuation palindromes,
  text palindromes surrounded by (if at all) non-letters.
  This requires a quadratic postprocessing step.
- Finding palindromes with substitution errors,
  in which a limited number of symbols may be substituted by other symbols to get palindromes. 
  Like 'river', which is a perfect palindrome if the 'i' is substituted for an 'e'. 
- Finding (approximate) palindromes with
  gaps in the center.
- This algorithm is faster than the approximate algorithm. It is also the fastest for most regular texts. 

The approximate palindrome algorithm further supports:
- Finding approximate palindromes with insertion, deletion and substitution errors,
  in which a limited number of insertion, deletion or substitution operations on the 'left arm' of the approximate palindrome results in a palindrome. A simple example is 'levels', which is a palindrome if you insert one 's' at the start of the word. 
- Finding (approximate) palindromes with
  gaps in the center.

The algorithms search for maximal palindromes but use different definitions of maximal. 
For the linear and quadratic algorithm a maximal palindrome is the possible palindrome from a center (a position on a letter or inbetween two letters), from which the palindrome is extended on both sides. This means the algorithm will find a palindrome for each center. In approximate algorithm a maximal palindrome is any substring which cannot be extended on one or both sides without exceeding the allowed amount of insertions, deletions or substitions. 

For more information on the different algorithms and the different kinds of palindromes, see the tutorial on [our website](https://palindromes.science.uu.nl/smallsites/tutorial.html).

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

run the following commands, with changing `<path-to-file-or-directory>` to the file or directory of files containing the input, and `<options>` with the flags you want enabled
```
    palindromes <path-to-file-or-directory> -<options>
```
Here are some examples of working flags, provided input.txt is a file in the same directory as the palindromes.cabal file:

```
    palindromes input.txt
    palindromes input.txt --quadratic --punctuation
    palindromes input.txt -Q
    palindromes input.txt -Q3+0
    palindromes input.txt -L --longest
    palindromes input.txt -A0+2 --all --details --minlength=6
    palindromes -i --dna
```

To see all the options run one of these:

```
    palindromes
    palindromes -h
    palindromes --help
```

If you want to see unicode characters in the output in a PowerShell terminal on Windows, try to use the following commands in the terminal:
```
    $OutputEncoding = [Console]::OutputEncoding = [Text.UTF8Encoding]::UTF8
```

When working on the package in a code editor you can use:
```
    cabal run palindromes --
```
Instead of 
```
    palindromes
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