{-
Computes the probability of observing a Schrodinger CRC using
QuickCheck-generated random data.

Lee Pike <lee-pike-@-gmail-.-com> (remove '-'s)
Copyright, 2009
-}

module Main

where

import System.Random.Mersenne

import Data.List
import Test.QuickCheck

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Gen

------------------------

-- Helper for Mersenne randoms for ints, generates a random in [low, high]
randomHlp :: (MTRandom a, Integral a) => (a, a) -> IO a
randomHlp (low, high) = do r <- randomIO::IO Double -- generates in [0, 1) 
                           return $ low + (round (r * (fromIntegral $ high - low)))

type Bits = Integer 

-- |Bitvector type (however, only lists of 0s and 1s are valid).
type BV = [Bits]

-- |Changes a bitstring (list of 0's and 1's), with the most sig bit at the
-- |head, to its corresponding natural number representation.
fromBV :: BV -> Integer
fromBV y = foldl (+) 0 $ zipWith (*) (reverse y) $ map (\x -> 2^x) $ [0..]

-- |Converts a natural into a bitstring (list of 0's and 1's), with the most sig
-- |bit at the head.
toBV :: Integer -> BV
toBV x = reverse $ toBVHelp 1 x
    where toBVHelp i rst =
              let n = 2^i
                  r = rem rst n
                  d = div rst n
              in if d == 0
                 then (if (r == 0) then [0] else [1])
                 else if (d == 1) && (r == 0)
                      then [0,1]
                      else if r == 0
                           then 0:(toBVHelp (i+1) rst)
                           else 1:(toBVHelp (i+1) (rst-r))

-- Sanity check.
prop_bitconvert x = x >= 0 ==> (fromBV . toBV) x == x

-- |Specific CRC polynomials (see
-- |<http://en.wikipedia.org/wiki/Cyclic_redundancy_check>).
ccitt, crc4itu,crc8atm, parity :: BV
ccitt = [1,1,0,0,0,1,1,0,1]
crc4itu = [1,0,0,1,1]
crc8atm = [1,0,0,0,0,0,1,1,1]
parity = [1,1]
usb5 = [1,0,0,1,0,1] -- Standard and optimal for words of 11 bits.  Koopman &
                     -- Chakravarty "CRC Polynomial Selection for Embedded
                     -- Networks," DSN 2004.
can = [1,1,0,0,0,1,0,1,1,0,0,1,1,0,0,1] -- For the CAN bus.  HD=6 up to 112-bit data words

-- |Removes the leading 0s from a bitvector, but don't return an empty bitvector.
shrinkBV :: BV -> BV
shrinkBV bv = let bv' = dropWhile (== 0) bv
              in if null bv' then [0] else bv'

-- |Takes a bitvector and the CRC polynomial and returns the bitvector divided
-- by the polynomial over GF(2).  For a polynomial of degree n, add n zeros.
crc :: BV -- ^ bitvector
    -> BV -- ^ polynomial
    -> BV
crc bv p = crc' (bv ++ (replicate (length p) 0)) p

crc' bv p = let x = (fromBV bv)
                bv' = shrinkBV bv
                xor = let z = p ++ [0, 0..]
                      in map (\(a,b) -> mod (a+b) 2) (zip bv' z)
            in if x < 2^(length p - 1) then bv'
               else crc' xor p 

-- |CRC check where the bitvector and polynomial are represented as naturals.
crcFromInt :: Integer -- ^ bitvector
       -> Integer -- ^ polynomial
       -> BV
crcFromInt nbv np = crc (toBV nbv) (toBV np)

-- Sanity check for parity.
prop_parity n = crcFromInt n (fromBV parity) == crc (toBV n ++ [1,1]) parity

-- | Takes a bitvector bv, a Hamming Distance hd, and returns a random bitvector
-- | such that some random i number of 0s in bv are flipped to 1, where i >= hd
-- | (and less than the total number of 0s!).  This function should only be
-- | called by bvs with at least hd 0s.
rndBv0to1 :: BV -> Int -> IO BV
rndBv0to1 bv hd =
    let idxs0s = elemIndices 0 bv
    in do i <- randomHlp (hd, length idxs0s) -- total number of bits to flip
          idxs <- whichBitsFlipped i idxs0s -- which bits are we flipping?
          return $ replaceBits bv idxs
              where whichBitsFlipped j xs =
                      if j == 0 then return []
                      else do r <- randomHlp (0, (length xs) - 1)
                              rst <- whichBitsFlipped (j-1) (delete (xs !! r) xs)
                              return $ (xs !! r):rst

-- | Takes a bitvector bv and a list of indices ls and returns a bitvector such
-- | that for each index in ls, the bit at that index is flipped.
replaceBits :: BV -> [Int] -> BV
replaceBits bv ls = let ls' = sort ls
                    in replaceBits' 0 ls'
    where replaceBits' k indicies =
              case indicies of [] -> drop k bv
                               i:is -> (drop k $ take i bv)
                                   ++ (bitFlip $ bv !! i):(replaceBits' (i+1) is)

-- |Flip a bit.
bitFlip :: Bits -> Bits
bitFlip x = if x == 1 then 0 else 1

-- |Takes a bitvector bv and polynomial, and computes the CRC.  It then randomly
-- |flips from 0s to 1s [hamming distance (HD) <= i <= # of zeros] in the frame
-- |check sequence (word + crc).  It returns false if the corrupted CRC is valid
-- |for the corrupted message.
schrodingerCRC :: BV -- ^ bitvector
               -> BV -- ^ crc polynomial
               -> Int -- ^ hamming distance
               -> Bool -- ^ show output
               -> IO Bool
schrodingerCRC bv poly hd out =
    let cksum = crc bv poly -- get the checksum on the original
        fcs = bv ++ cksum
    in if (length $ filter (== 0) fcs) < hd
       then do (if out then putStrLn $ "0s less than Hamming Distance: " ++ (show fcs)
                else return ())
               return True
       else do fcs' <- rndBv0to1 fcs hd -- flip some bits from 0 to 1
               let rcvcksum = drop (length bv) fcs'
                   bv' = take (length bv) fcs'
                   cksum' = crc bv' poly
                  in do if out
                          then do putStrLn $ "Original bitvector: " ++ (show bv)
                                               ++ "  Original checksum: " ++ (show cksum)
                                  putStrLn $ "Received bitvector: "
                                               ++ (show bv') ++ "  Received checksum: " ++ (show rcvcksum)
                                  putStrLn $ "Actual checksum: " ++ (show cksum')
                          else return ()
                        return $ rcvcksum /= cksum' 

prop_noschrodinger poly i hd out =
  monadicIO $ do rnd <- pick $ genBVs i
                 b <- run $ schrodingerCRC (toBV rnd) poly hd out
                 assert b

-- Generates a random BV up to some length i.
genBVs :: Integer -> Gen Integer
genBVs i = choose (0::Integer, (2^i-1))

-- Execute
main :: IO ()
-- ~3.29e-2 == 0.0329 
main = quickCheckQuotientWith (stdArgs {maxSuccess = 10000}) (prop_noschrodinger usb5 11 3 False)

-- result: ~39/1000000 == 3.9e-5
--main = quickCheckQuotientWith (stdArgs {maxSuccess = 1000000}) (prop_noschrodinger can 64 6 False)

-------------------
-- Functions for calculating the probability of bit corruptions.

-- Factorial.
fac :: Integer -> Integer
fac n = product [1..n]

top :: Integer -> Integer
top n = round $ (fromIntegral n)/2

prob :: Double -> Integer -> Integer -> Double
prob p flipped n = p^flipped * (1-p)^(n - flipped)

choose' :: Integer -> Integer -> Double
choose' flipped n =
    fromRational $ (fromIntegral (fac n)) / (fromIntegral ((fac flipped) * fac (n - flipped)))

summation :: Double -> Integer -> Integer -> Double
summation p hd n =
    sum $ map f [hd..t]
        where t = top n
              f = \x -> (prob p x t) * (choose' x t)
