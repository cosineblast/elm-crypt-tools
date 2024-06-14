
port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import HashAlgorithm exposing (..)

main = Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }

-- Ports

port askHash : (String, String) -> Cmd msg
port onHash : (String -> msg) -> Sub msg

-- Structure

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
        SwitchAlgorithm (name) ->
            case parseAlgorithm name of
                Nothing -> { model | algorithm = Nothing, computedHash = Nothing } |> pure
                Just algo -> ({ model | algorithm = Just algo, computedHash = Nothing }, askHash (name, model.inputText))

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
                ] (
                    [ option [selected True, disabled True, value ""] [ text "Algorithm"] ] ++
                    List.map (\x -> let name = algorithmName x in
                        option [ (value name)] [text name]
                        )
                    allAlgorithms),

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
