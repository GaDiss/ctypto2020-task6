# Task 6
ITMO Group project №1 for 2020 Cryptography course from 
[HW page](http://neerc.ifmo.ru/teaching/crypto/year2018/tasks/task6.html)

### Improving Authenticated Dynamic Dictionaries, with Applications to Cryptocurrencies

Simplified for studying purposes haskell implementation of 
an improved dynamic dictionary data structure
described [here](https://eprint.iacr.org/2016/994.pdf)

##### Prover
Prover implemented as a simple function that can be denoted as follows

```Haskell
-- | applies operation and generates proof
prove :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
      => AVLTree key value       -- ^ Old Tree
      -> Operation key value     -- ^ Operation
      -> ( [ProofNode key value] -- ^ Proof of the operation
         , AVLTree key value     -- ^ New Tree
         , Maybe value           -- ^ Operation return value
         )
```
##### Verifier
Verifier implemented as a simple function that can be denoted as follows

```Haskell
-- | verifies proof of a given operation
verify :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
       => RootDigest            -- ^ old digest
       -> Operation key value   -- ^ applied operation
       -> [ProofNode key value] -- ^ proof
       -> ( Result              -- ^ verifier's verdict
          , RootDigest          -- ^ new digest
          , Maybe value         -- ^ return value
          )
```