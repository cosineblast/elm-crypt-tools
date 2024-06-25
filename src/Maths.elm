module Maths exposing (..)

import BigInt
import BigInt exposing (BigInt)


type alias Integer =
    BigInt


stringToInteger : String -> Maybe Integer
stringToInteger =
    BigInt.fromIntString


integerToString : Integer -> String
integerToString =
    BigInt.toString

zero : Integer
zero = BigInt.fromInt 0

one : Integer
one = BigInt.fromInt 1

-- This BigInt library's modBy operation is more like haskell's `rem`;
-- It returns negative values, so we use this version of modBy instead.
modBy_ : Integer -> Integer -> Maybe Integer
modBy_ m a =
    BigInt.modBy m a
    |> Maybe.map (\it -> if BigInt.lt it zero then BigInt.add m it else it)

extendedEuclid : Integer -> Integer -> Maybe { gcd: Integer, m: Integer, n: Integer }
extendedEuclid a b =
    let z = zero
        i = one
        step  q0 r0 s0 t0
              q1 r1 s1 t1 =
                if r1 /= z then
                    let ( qn, rn ) = BigInt.divmod r0 r1 |> Maybe.withDefault ( z, z )
                        sn = BigInt.sub s0 (BigInt.mul qn s1)
                        tn = BigInt.sub t0 (BigInt.mul qn t1)
                    in step q1 r1 s1 t1
                            qn rn sn tn
                else { gcd = r0, m = s0, n = t0 }
    in if a == z || b == z then
        Nothing
        else Just (step z a i z
                        z b z i)


modularInverse : Integer -> Integer -> Maybe Integer
modularInverse a modulus =
    extendedEuclid a modulus
    |> Maybe.andThen
        (\result ->
            if result.gcd /= one then
                Nothing
            else modBy_ modulus result.m
        )


modularPow : Integer -> Integer -> Integer -> Maybe Integer
modularPow x y z =
    BigInt.pow x y
    |> modBy_ z

computePow : Integer -> Integer -> Integer -> Maybe Integer
computePow x y z =
    if BigInt.gte y zero then
        modularPow x y z
    else modularInverse x z |> Maybe.andThen (\it -> modularPow it (BigInt.abs y) z)


