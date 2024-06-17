module TriMaybe exposing (..)


type TriMaybe a
    = Empty
    | Invalid
    | Valid a


isInvalid : TriMaybe a -> Bool
isInvalid m =
    case m of
        Invalid ->
            True

        _ ->
            False


fromMaybeInvalid : Maybe a -> TriMaybe a
fromMaybeInvalid m =
    case m of
        Nothing ->
            Invalid

        Just x ->
            Valid x
