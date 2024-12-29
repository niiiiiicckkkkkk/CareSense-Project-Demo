{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Binary where

{-
A Module for Binary Data

Binary values are defined as arrays of Bit 
and interpreted as signed 2's complement by default

They are grouped into families of equal width by a phantom type of kind Nat

Implements Num, Ord, Eq and Ix for use in the Simulator module

Additional support for projection and appending of binary values provided the types align
See append and extract

-}

import Data.Word
import Data.Bits
import Data.Ix
import Data.Monoid
import Data.Proxy
import GHC.TypeLits
import Data.Array ( assocs, elems, array, listArray, Array, (!) )
import Data.List (elemIndex)
import Test.HUnit ( (~?=), Test(TestList), Counts, runTestTT)
import Data.Word(Word8)


type Width = Nat

data Bit = Lo | Hi deriving (Eq, Ord, Show)

newtype Binary (width :: Width) = Binary {getBinary :: Array Integer Bit} 
    deriving Show

-- Bits support the expected bitwise operations
negB :: Bit -> Bit
negB Lo = Hi
negB Hi = Lo

andB :: Bit -> Bit -> Bit
andB b b'
    | Hi <- b
    , Hi <- b' = Hi
    | otherwise = Lo

orB :: Bit -> Bit -> Bit
orB b b'
    | Lo <- b
    , Lo <- b' = Lo
    | otherwise = Hi

xorB :: Bit -> Bit -> Bit
xorB b b'
    | Lo <- b
    , Hi <- b' = Hi
    | Hi <- b
    , Lo <- b' = Hi
    | otherwise = Lo

bounds :: forall (w :: Width) a . (KnownNat w, Num a) => (a, a)
bounds =
    let upperBound = natVal (Proxy @w) - 1
    in (0, fromIntegral upperBound)

-- type safe binary projection
extract :: forall lo hi w1 w2 . 
            ( KnownNat w1
            , KnownNat w2
            , KnownNat lo
            , KnownNat hi
            , CmpNat lo w1 ~ 'LT
            , CmpNat hi w1 ~ 'LT
            , CmpNat lo hi ~ 'LT
            , w2 ~ hi - lo + 1
            ) => Binary w1 -> Binary w2
extract (Binary bits) =
    let indices = [natVal (Proxy @lo)..natVal (Proxy @hi)]
    in Binary 
        $ listArray (bounds @w2)
        $ [  bits ! i  | i <- indices]

-- type safe binary combination
append :: forall w1 w2 w3.
            ( KnownNat w1
            , KnownNat w2
            , KnownNat w3
            , w3 ~ (w1 + w2)
            ) => Binary w1 -> Binary w2 -> Binary w3
append (Binary bits) (Binary bits') =
    Binary $ listArray (bounds @w3) $ elems bits' <> elems bits

isDivisibleBy :: (Integral a) => a -> a -> Bool
isDivisibleBy x y = x `mod` y == 0

isNegative :: forall w. KnownNat w => Binary w -> Bool
isNegative b =
    let signBit = fromInteger $ natVal (Proxy @w) - 1
    in testBit b signBit

uBinToInteger :: forall w. KnownNat w => Binary w -> Integer
uBinToInteger b = foldr ((+) . aux) 0 $ assocs (getBinary b)
    where
        aux :: (Num a, Ix i, Integral i) => (i, Bit) -> a
        aux (i, Hi) = 2 ^ i
        aux (i, Lo) = 0

sBinToInteger :: forall w. KnownNat w => Binary w -> Integer
sBinToInteger b
    | isNegative b = -(uBinToInteger (negate b))
    | otherwise = uBinToInteger b
    

bitwiseBinop :: forall w. KnownNat w => 
                (Bit -> Bit -> Bit) 
                -> Binary w 
                -> Binary w 
                -> Binary w
bitwiseBinop op (Binary bits) (Binary bits') =
    Binary 
    $ listArray (bounds @w)
    $ zipWith op (elems bits) (elems bits')

signExtend :: KnownNat w => Binary w -> [Bit]
signExtend b
    | isNegative b = elems (getBinary b) <> repeat Hi
    | otherwise = elems (getBinary b) <> repeat Lo


instance KnownNat w => Eq (Binary (w :: Width)) where
    (==) :: KnownNat w => Binary w -> Binary w -> Bool
    (==) (Binary b) (Binary b') = elems b == elems b'

instance KnownNat w => Ord (Binary (w :: Width)) where
    compare :: Binary w -> Binary w -> Ordering
    compare b b'
        | isNegative b
        , (not . isNegative) b' = LT
        | isNegative b'
        , (not . isNegative) b = GT
        | isNegative b
        , isNegative b' = compare (negate b') (negate b)
        | otherwise =
            let normalizeBits = (reverse . elems . getBinary)
            in compare (normalizeBits b) (normalizeBits b')


instance KnownNat w => Num (Binary (w :: Width)) where
    (+) :: Binary w -> Binary w -> Binary w
    (+) b b' = fromIntegral $ uBinToInteger b + uBinToInteger b'

    (*) :: Binary w -> Binary w -> Binary w
    (*) b b' = fromIntegral $ uBinToInteger b * uBinToInteger b'

    negate :: Binary w -> Binary w
    negate = (+1) 
            . Binary 
            . listArray (bounds @w)
            . map negB 
            . elems 
            . getBinary

    abs :: Binary w -> Binary w
    abs b
        | isNegative b = negate b
        | otherwise = b

    signum :: Binary w -> Binary w
    signum b
        | b == 0 = 0
        | b > 0 = 1
        | otherwise = -1

    fromInteger :: Integer -> Binary w
    fromInteger n 
        | n > 0 = Binary $ listArray (bounds @w) $ go n 0 <> repeat Lo
        | n < 0 = negate . fromInteger . negate $ n
        | otherwise = Binary $ listArray (bounds @w) $ repeat Lo
        where
            go :: Integral a => a -> a -> [Bit]
            go n i
                | digit > n = error "broken invariant"
                | digit == n = [Hi]
                | n `isDivisibleBy` nextDigit = Lo : go n (i + 1)
                | otherwise = Hi : go (n - digit) (i + 1)
                where
                    digit = 2 ^ i
                    nextDigit = digit * 2


instance KnownNat w => Bits (Binary (w :: Width)) where
    (.&.) :: Binary w -> Binary w -> Binary w
    (.&.) = bitwiseBinop andB

    (.|.) :: Binary w -> Binary w -> Binary w
    (.|.) = bitwiseBinop orB

    xor :: Binary w -> Binary w -> Binary w
    xor = bitwiseBinop xorB

    complement :: Binary w -> Binary w
    complement (Binary bits) =
        Binary
        $ listArray (bounds @w)
        $ negB <$> elems bits

    shift :: Binary w -> Int -> Binary w
    shift b@(Binary bits) i
        | i > 0 = Binary 
                $ listArray (bounds @w)
                $ replicate i Lo <> elems bits
        | i < 0 = Binary
                $ listArray (bounds @w)
                $ drop (abs i) (signExtend b)
        | otherwise = Binary bits

    rotate :: Binary w -> Int -> Binary w
    rotate (Binary bits) i =
        let f idx = (idx + fromIntegral i) `mod` natVal (Proxy @w)
        in Binary $ array (bounds @w) [ (f idx, v) | (idx, v) <- assocs bits]

    bitSize :: Binary w -> Int
    bitSize _ = fromInteger $ natVal (Proxy @w)

    bitSizeMaybe :: Binary w -> Maybe Int
    bitSizeMaybe = Just . bitSize

    isSigned :: Binary w -> Bool
    isSigned = const True

    testBit :: Binary w -> Int -> Bool
    testBit b n = b .&. bit n /= 0

    bit :: Int -> Binary w
    bit n 
        | n >= 0 = Binary $ listArray (bounds @w) $ go n <> repeat Lo
        | otherwise = bit . fromInteger $ natVal (Proxy @w) + toInteger n
        where
            go :: Int -> [Bit]
            go 0 = [Hi]
            go n = Lo : go (n - 1)


    popCount :: Binary w -> Int
    popCount = length . filter (==Hi) . elems .getBinary

{-
Todo: figure out if my signed binary type can be a lawful inhabitant of the Enum class

instance KnownNat w => Enum (Binary (w :: Width)) where
    toEnum :: Int -> Binary w
    toEnum = fromIntegral

    fromEnum :: Binary w -> Int
    fromEnum = fromInteger . sBinToInteger
-}

instance KnownNat w => Ix (Binary (w :: Width)) where
    range :: (Binary w, Binary w) -> [Binary w]
    range (b, b')
        | b == b' = [b]
        | otherwise = b : range (b + 1, b')

    index :: (Binary w, Binary w) -> Binary w -> Int
    index (lo, hi) b =
        case b `elemIndex` range (lo, hi) of
            Just i -> i
            Nothing -> error "index out of bounds"

    inRange :: (Binary w, Binary w) -> Binary w -> Bool
    inRange (lo, hi) b = b `elem` range (lo, hi)


test_binary :: IO Counts
test_binary = do
    runTestTT $
        TestList 
        [ identityTest
        , additionTest
        , bitRepTest
        , rangeTest
        ]
    where
        identityTest = sBinToInteger @8 . fromInteger <$> [-128..127] ~?= [-128..127]
        additionTest = 
            let nums = fromInteger @(Binary 4) <$> [-8..7]
            in (+) <$> nums <*> nums ~?= fromInteger <$> ((+) <$> [-8..7] <*> [-8..7])
        bitRepTest =
            let binrep = do
                    num <- fromInteger @(Binary 8) <$> [-128..127]
                    index <- [0..7]
                    return $ testBit num index
                wordrep = do
                    num <- fromInteger @(Word8) <$> [-128..127]
                    index <- [0..7]
                    return $ testBit num index 
            in binrep ~?= wordrep
        rangeTest = sBinToInteger @8 <$> range (-128, 127) ~?= range (-128, 127)

