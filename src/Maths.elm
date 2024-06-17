
module Maths exposing (..)

import BigInt exposing (BigInt)

type alias Integer = BigInt

stringToInteger : String -> Maybe Integer
stringToInteger = BigInt.fromIntString

integerToString : Integer -> String
integerToString = BigInt.toString

computePow : Integer -> Integer -> Integer -> Integer
computePow x y z =
    case BigInt.pow x y |> BigInt.modBy z of
        Nothing -> BigInt.fromInt 0
        Just v -> v
