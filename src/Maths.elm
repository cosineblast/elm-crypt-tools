module Maths exposing (..)

import BigInt
import BigInt as B
import BigInt exposing (BigInt)


type alias Integer =
    BigInt


stringToInteger : String -> Maybe Integer
stringToInteger =
    B.fromIntString


integerToString : Integer -> String
integerToString =
    B.toString

zero : Integer
zero = B.fromInt 0

one : Integer
one = B.fromInt 1

-- This BigInt library's modBy operation is more like haskell's `rem`;
-- It returns negative values, so we use this version of modBy instead.
modBy_ : Integer -> Integer -> Maybe Integer
modBy_ m a =
    B.modBy m a
    |> Maybe.map (\it -> if B.lt it zero then B.add m it else it)

extendedEuclid : Integer -> Integer -> Maybe { gcd: Integer, m: Integer, n: Integer }
extendedEuclid a b =
    let z = zero
        i = one
        step  q0 r0 s0 t0
              q1 r1 s1 t1 =
                if r1 /= z then
                    let ( qn, rn ) = B.divmod r0 r1 |> Maybe.withDefault ( z, z )
                        sn = B.sub s0 (B.mul qn s1)
                        tn = B.sub t0 (B.mul qn t1)
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
    B.pow x y
    |> modBy_ z

computePow : Integer -> Integer -> Integer -> Maybe Integer
computePow x y z =
    if B.gte y zero then
        modularPow x y z
    else modularInverse x z |> Maybe.andThen (\it -> modularPow it (B.abs y) z)


