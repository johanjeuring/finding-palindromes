Release history:

--------
25062025 Version 1.1.0.0
--------
- Define internal modules to signal which modules are and are not part of the public interface.
- Add file with test strings instead of depending on text files.
- Rename functions in quadratic algorithm.
- Update documentation.

--------
19062025 Version 1.0.0.1
--------
Fix the cabal-version from 3.12 to 3.4, because 3.12 is not supported on Hackage.

--------
19062025 Version 1.0
--------
Features:
- Add approximate palindrome algorithm. 
- Add benchmarking and profiling.
- Optimizations to the finding algorithms.
- Add --details and --ranges flags.
- Directory can now be input.
- Add progress bar.

Fixes:
- Support odd gapped palindromes for DNA.
- Seperate executable from library.
- Improved flag names.
- Remove maxLength option.
- Rename 'Complexity' datatype to 'Algorithms'

--------
07042025 Version 0.5
--------
Does a big clean up of the whole package:
- Generalize the input data type. Any abstract data type of the PalEq class, with the (=:=) operator, can be the input for the palindrome finding algorithms.
- Define a new output type. This can be found in src\Data\Algorithms\Palindromes\Palindrome.hs.
- Rewrite functions to be clearer.
- Split the package into more modules.
- Split the pre- and post-processing from the algorithms themselves. This allows for adding more pre- and postprocessing easily in the future.
- Add Haddock documentation. Every function has a Haddock-style comment, allowing Haddock to make automatic documentation for the package.
- Add support for word palindromes.
- Change the way punctuation palindromes are found. Every punctuation palindrome is a substring of the maximal palindrome at the same center. So, we find punctuation palindromes easily by shrinking maximal palindromes until we have a punctuation palindrome, solving this problem with a post-processing solution.
- Add many unit tests.
- Add QuickCheck generators for texts with (large) palindromes and properties for palindromes.

--------
08072012 Version 0.4
--------
Cleaned up the options, to make them available for other kinds
of palindromes besides DNA palindromes. Code size has been reduced
by more than 40% by using higher-order functions and laziness to
properly deal with variability.

--------
08072012 Version 0.3.2
--------
Only maximal palindromes are shown by the maximalEvenPalindromesLengthBetweenDNA
function.

--------
03072012 Version 0.3.1
--------
Also showing length in DNA palindromes.

--------
01072012 Version 0.3
--------
Uses Data.Bytestring instead of String
Includes functionality for determining palindromes in DNA
Added many flags for determining the length of the palindromes returned

--------
19032012 Version 0.2.2.2
--------
Corrects a non-critical error in finalWordCentres

--------
19032012 Version 0.2.2.1
--------
Corrects a link

--------
17032012 Version 0.2.2
--------
Corrects the word palindromes solution

--------
26122011 Version 0.2.1
--------
Updates base dependency from <=4 to <5
Uses latin1 character set for input files

--------
10012010 Version 0.2
--------
Reads from standard input, via the flag -i
More flexible flag handling
Reads multiple files
Specifies minimum length of palindromes returned, via the flag -m int

--------
07092009 Version 0.1.1
--------
Corrects two errors in the flags

--------
06092009 Version 0.1
--------
First version of the package


This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.

© Copyright Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring