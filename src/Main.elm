
module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

main = Browser.sandbox { init = init, view = view, update = update }

type HashAlgorithm = SHA256 | SHA1

algorithmName : HashAlgorithm -> String
algorithmName algo = case algo of
    SHA256 -> "SHA256"
    SHA1 -> "SHA1"

type alias Model = {
        algorithm: Maybe HashAlgorithm,
        inputText: String
    }

type Msg = SwitchAlgorithm String
    | InputTyped String


init : Model
init = {
    algorithm = Nothing,
    inputText = ""
    }

update : Msg -> Model -> Model
update message model =
    case message of
        SwitchAlgorithm (algo) -> case algo of
            "SHA256" -> { model | algorithm = Just SHA256 }
            "SHA1" -> { model | algorithm = Just SHA1 }
            _ -> { model | algorithm = Nothing }

        InputTyped (str) -> { model | inputText = str }


view : Model -> Html Msg
view model = div []
    [ main_ [ class "container" ]
        [   h1 [] [ text "Cryptool" ],

            div [] [

                h4 [] [ text "Compute Hash" ],

                select [name "algo", attribute "aria-label" "Algorithm",
                    onInput SwitchAlgorithm
                ] [
                    option [selected True, disabled True, value ""]
                    [ text "Algorithm"],

                    option [value "SHA256"] [ text "SHA256" ],
                    option [value "SHA1"] [ text "SHA1" ]
                ],

                input [placeholder "Type some input", onInput InputTyped ] [  ],

                case model.algorithm of
                    Nothing -> div [] []
                    (Just algo) -> div []
                        [ algo |> algorithmName |> text,
                          text " Hash: ",
                          code [] [
                              text ("hash(" ++ algorithmName algo ++ "," ++ model.inputText ++ ")")
                          ]
                        ]
            ]

        ]
    ]
