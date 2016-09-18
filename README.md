Lognum
------

This package exports `LB`, a type for doing computations in logbase using
common Haskell typeclasses.

    ghci
    > import Numeric.LogBase
    > 0.5 :: LB Double
    <LB -0.6931471805599453 --> 0.5>
    > 0.1**1000 :: LB Double
    <LB -2302.585092994045 --> 0.0>
    > (0.1**1000)**(1/1000) :: LB Double
    <LB -2.302585092994046 --> 9.999999999999998e-2>

License
-------

ISC licensed. See LICENSE.

