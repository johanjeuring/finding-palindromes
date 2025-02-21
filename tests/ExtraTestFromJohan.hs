{-| ----------------------------------------------------------------------------
%
%  Title       :  Finding palindromes: variants and algorithms
%  Event       :  Liber Amicorum Rinus Plasmeijer
%  Author(s)   :  Johan Jeuring
%  Copyright   :  (c) 2013 Springer
%  Created     :  29 November 2012
%
---------------------------------------------------------------------------- | -}
> testPalindrome1 = 
>   TestCase (assertEqual 
>               "palindrome1" 
>               True 
>               (palindrome "abbadabba")
>            )
> 
> testPalindrome2 = 
>   TestCase (assertEqual 
>               "palindrome2" 
>               False 
>               (palindrome "abbadabca")
>            )
>
> testPalindromeEmpty = 
>   TestCase (assertEqual 
>               "palindromeEmpty" 
>               True 
>               (palindrome "")
>            )
> 
> testPalindromeSingle = 
>   TestCase (assertEqual 
>               "palindromeSingle" 
>               True 
>               (palindrome "a")
>            )
>
> testTextPalindrome1 = 
>   TestCase (assertEqual 
>               "textPalindrome1" 
>               True 
>               (textPalindrome "A man, a plan, a canal, Panama!")
>            )
>
> testTextPalindrome2 = 
>   TestCase (assertEqual 
>               "textPalindrome2" 
>               False 
>               (textPalindrome "draweth toward")
>            )
>
> testWordPalindrome1 = 
>   TestCase (assertEqual 
>               "wordPalindrome1" 
>               True 
>               (wordPalindrome ("","war draw",""))
>            )
>
> testWordPalindrome2 = 
>   TestCase (assertEqual 
>               "wordPalindrome2" 
>               False 
>               (wordPalindrome ("","no man; even amon","g"))
>            )
>
> testDNAPalindrome1 = 
>   TestCase (assertEqual 
>               "dnaPalindrome1" 
>               True 
>               (dnaPalindrome "AAGCGCTT")
>            )
>
> testDNAPalindrome2 = 
>   TestCase (assertEqual 
>               "dnaPalindrome2" 
>               False 
>               (dnaPalindrome "AACGGCAA")
>            )
>
> testApproximatePalindrome1 = 
>   TestCase (assertEqual 
>               "approximatePalindrome1" 
>               True 
>               (approximatePalindrome 2 "marm")
>            )	
>
> testApproximatePalindrome2 = 
>   TestCase (assertEqual 
>               "approximatePalindrome2" 
>               False 
>               (approximatePalindrome 1 "marm")
>            )
>
> testGappedPalindrome1 = 
>   TestCase (assertEqual 
>               "gappedPalindrome1" 
>               True 
>               (gappedPalindrome 3 "gogandmagog")
>            )	
>
> testGappedPalindrome2 = 
>   TestCase (assertEqual 
>               "gappedPalindrome2" 
>               False 
>               (gappedPalindrome 1 "gogandmagog")
>            )
>
> propEmpty :: (String -> Bool) -> Bool
> propEmpty pal = pal "" == True
>
> propSingle :: (String -> Bool) -> Property
> propSingle pal = 
>   forAll (arbitrary :: Gen Char) $
>     \c -> pal [c]
>
> propExtend :: (String -> Bool) -> Property
> propExtend pal =
>   forAll (arbitrary :: Gen [Char]) $
>     \xs -> forAll (arbitrary :: Gen [Char]) $
>        \ys -> pal ys == pal (xs ++ ys ++ reverse xs) 
>
> propExtendDNA :: Property
> propExtendDNA =
>   forAll (listOf (elements "ACTG") :: Gen [Char]) $
>     \xs -> forAll (listOf (elements "ACTG")  `suchThat` (even . length) :: Gen [Char]) $
>        \ys -> dnaPalindrome ys == dnaPalindrome (xs ++ ys ++ map revert (reverse xs)) 
>
> propExtendGap :: Int -> Property
> propExtendGap n =
>   forAll (arbitrary :: Gen [Char]) $
>     \xs -> forAll (arbitrary `suchThat` (\l -> (length l `mod` 2 == n `mod` 2) && length l >= n) :: Gen [Char]) $
>        \ys -> gappedPalindrome n ys == gappedPalindrome n (xs ++ ys ++ reverse xs) 
>
> revert 'A' = 'T'
> revert 'T' = 'A'
> revert 'C' = 'G'
> revert 'G' = 'C'
> revert c   = error (show c ++ " should not appear here")
>
> tests :: Test
> tests = TestList [TestLabel "palindrome1"      testPalindrome1
>                  ,TestLabel "palindrome2"      testPalindrome2
>                  ,TestLabel "palindromeEmpty"  testPalindromeEmpty
>                  ,TestLabel "palindromeSingle" testPalindromeSingle
>                  ,TestLabel "textpalindrome1"  testTextPalindrome1
>                  ,TestLabel "textpalindrome2"  testTextPalindrome2
>                  ,TestLabel "wordpalindrome1"  testWordPalindrome1
>                  ,TestLabel "wordpalindrome2"  testWordPalindrome2
>                  ,TestLabel "dnapalindrome1"   testDNAPalindrome1
>                  ,TestLabel "dnapalindrome2"   testDNAPalindrome2
>                  ,TestLabel "apppalindrome1"   testApproximatePalindrome1
>                  ,TestLabel "apppalindrome2"   testApproximatePalindrome2
>                  ,TestLabel "gappalindrome1"   testGappedPalindrome1
>                  ,TestLabel "gappalindrome2"   testGappedPalindrome2
>                  ]
> 
> main :: IO Counts
> main = do quickCheck (propEmpty palindrome)
>           quickCheck (propSingle palindrome)
>           quickCheck (propExtend palindrome)
>           quickCheck (propEmpty textPalindrome)
>           quickCheck (propSingle textPalindrome)
>           quickCheck (propExtend textPalindrome)
>           quickCheck (propEmpty dnaPalindrome)
>           quickCheck (propSingle dnaPalindrome)
>           quickCheck propExtendDNA
>           quickCheck (propEmpty (approximatePalindrome 0))
>           quickCheck (propSingle (approximatePalindrome 0))
>           quickCheck (propEmpty (gappedPalindrome 0))
>           quickCheck (propSingle (gappedPalindrome 1))
>           quickCheck (propExtendGap 3) 
>           runTestTT tests