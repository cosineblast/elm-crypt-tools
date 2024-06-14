
port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

main = Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


-- Ports

port askHash : (String, String) -> Cmd msg
port onHash : (String -> msg) -> Sub msg

-- Structure

type HashAlgorithm = SHA256 | SHA1
algorithmName : HashAlgorithm -> String
algorithmName algo = case algo of
    SHA256 -> "SHA256"
    SHA1 -> "SHA1"

type alias Model = {
        algorithm: Maybe HashAlgorithm,
        inputText: String,
        computedHash: Maybe String
    }

type Msg =
    SwitchAlgorithm String
    | InputTyped String
    | HashComputed String


init : () -> ( Model, Cmd Msg )
init _ = {
    algorithm = Nothing,
    inputText = "",
    computedHash = Nothing
    } |> pure




update : Msg -> Model -> (Model, Cmd Msg)
update message model =
    case message of
        SwitchAlgorithm (algo) -> case algo of
            "SHA256" -> { model | algorithm = Just SHA256 } |> pure
            "SHA1" -> { model | algorithm = Just SHA1 } |> pure
            _ -> { model | algorithm = Nothing } |> pure

        InputTyped (str) -> ({ model | inputText = str },
            case model.algorithm of
                Nothing -> Cmd.none
                Just algo -> askHash (algorithmName algo, str))

        HashComputed (str) -> { model | computedHash = Just str } |> pure



subscriptions : Model -> Sub Msg
subscriptions _ = onHash HashComputed


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

                input [placeholder "Type some input", onInput InputTyped] [  ],

                case model.algorithm of
                    Nothing -> div [] []
                    Just algo -> div []
                        [ algo |> algorithmName |> text,
                          text " Hash: ",
                          case model.computedHash of
                              Nothing -> text "...computing"
                              Just str -> code [] [text str]
                        ]
            ]
        ]
    ]

-- decoders

-- utility

pure : a -> (a, Cmd msg)
pure x = (x, Cmd.none)
