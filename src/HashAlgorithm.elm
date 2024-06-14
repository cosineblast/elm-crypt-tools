
module HashAlgorithm exposing (..)

type HashAlgorithm = SHA256 | SHA1

allAlgorithms : List HashAlgorithm
allAlgorithms = [SHA256, SHA1]

algorithmName : HashAlgorithm -> String
algorithmName algo = case algo of
    SHA256 -> "SHA256"
    SHA1 -> "SHA1"

parseAlgorithm : String -> Maybe HashAlgorithm
parseAlgorithm str =
    case str of
        "SHA256" -> Just SHA256
        "SHA1" -> Just SHA1
        _ -> Nothing
