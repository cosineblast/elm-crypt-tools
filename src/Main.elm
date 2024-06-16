
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

port askHmac : { message: String, key: String, algorithm: String } -> Cmd msg
port onHmac : (String -> msg) -> Sub msg

-- Structure

type alias Model = {
    hash: HashModel,
    hmac: HmacModel,
    pow: PowModel
    }

type alias HashModel = {
        algorithm: Maybe HashAlgorithm,
        input: String,
        computedHash: Maybe String
    }

type alias HmacModel = {
        algorithm: Maybe HashAlgorithm,
        message: String,
        key: String,
        computedHash: Maybe String
    }

type alias PowModel = {
        base: Int,
        exponent: Int,
        modulo: Int
    }

toHash : Model -> (HashModel -> HashModel) -> Model
toHash model f = { model | hash = f model.hash }

type Msg =
    SwitchHashAlgorithm String |
    HashInputTyped String |
    HashComputed String |

    SwitchHmacAlgorithm String |
    HmacInputTyped String |
    HmacKeyTyped String |
    HmacComputed String |

    PowBaseTyped String |
    PowExponentTyped String |
    PowModuloTyped String



init : () -> ( Model, Cmd Msg )
init _ = {
    hash = {
        algorithm = Nothing,
        input = "",
        computedHash = Nothing
    },
    hmac = {
        algorithm = Nothing,
        message = "",
        key = "",
        computedHash = Nothing
    },
    pow = {
        base = 1,
        exponent = 1,
        modulo = 1
    }
    } |> pure


updateHashModel : Msg -> HashModel -> (HashModel, Cmd Msg)
updateHashModel message model =
    case message of
        SwitchHashAlgorithm (name) ->
            case parseAlgorithm name of
                Nothing -> { model | algorithm = Nothing, computedHash = Nothing } |> pure
                Just algo -> ({ model | algorithm = Just algo, computedHash = Nothing }, askHash (name, model.input))

        HashInputTyped str -> ({ model | input = str },
            case model.algorithm of
                Nothing -> Cmd.none
                Just algo -> askHash (algorithmName algo, str))

        HashComputed str -> ({ model | computedHash = Just str }) |> pure

        _ -> model |> pure

updateHmacModel : Msg -> HmacModel -> (HmacModel, Cmd Msg)
updateHmacModel message model =
    case message of
        SwitchHmacAlgorithm name ->
            case parseAlgorithm name of
                Nothing -> { model | computedHash = Nothing } |> pure
                Just algo -> ({ model | algorithm = Just algo, computedHash = Nothing},
                    askHmac { algorithm = name, message = model.message, key = model.key })
        HmacInputTyped str -> ({ model | message = str},
            case model.algorithm of
                Nothing -> Cmd.none
                Just algo -> askHmac {algorithm = algorithmName algo, message = str, key = model.key})

        HmacKeyTyped str -> ({ model | key = str},
            case model.algorithm of
                Nothing -> Cmd.none
                Just algo -> askHmac {algorithm = algorithmName algo, message = model.message, key = str})

        HmacComputed str -> { model | computedHash = Just str } |> pure

        _ -> model |> pure

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    if isHashMsg msg then
        updateHashModel msg model.hash
        |> Tuple.mapFirst (\hash -> { model | hash = hash})
        else updateHmacModel msg model.hmac
        |> Tuple.mapFirst (\thing -> { model | hmac = thing})

isHashMsg : Msg -> Bool
isHashMsg msg =
    case msg of
        SwitchHashAlgorithm _ -> True
        HashInputTyped _ -> True
        HashComputed _ -> True
        _ -> False



subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch [onHash HashComputed, onHmac HmacComputed]


algorithmPickView : (String -> msg) -> Html msg
algorithmPickView switch = div [] [
    select [ name "algo",
             attribute "aria-label" "Algorithm",
             onInput switch
            ] (
                [ option [selected True, disabled True, value ""] [ text "Algorithm"] ] ++
                List.map (\x -> let name = algorithmName x in
                    option [ (value name)] [text name])
                allAlgorithms) ]

hashView : HashModel -> Html Msg
hashView model =
            section [] [

                h4 [] [ text "Compute Hash" ],

                algorithmPickView SwitchHashAlgorithm,

                input [placeholder "Input", onInput HashInputTyped] [  ],

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

hmacView : HmacModel -> Html Msg
hmacView model =
    section [] [
        h4 [] [text "HMAC"],

        algorithmPickView SwitchHmacAlgorithm,
        input [placeholder "Message", onInput HmacInputTyped] [],
        input [placeholder "Key", onInput HmacKeyTyped] [],

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

powView : PowModel -> Html Msg
powView model =
    div [] [
        h4 [] [ text "Modular Exponentiation" ],

        input [ placeholder "Base", onInput PowBaseTyped ] [],
        input [ placeholder "Exponent", onInput PowExponentTyped ] [],
        input [ placeholder "Modulo", onInput PowModuloTyped ] []
    ]


view : Model -> Html Msg
view model = div []
    [ main_ [ class "container" ]
        [
            h1 [] [ text "Cryptool" ],
            hashView model.hash,
            hmacView model.hmac,
            powView model.pow
        ]
    ]

-- decoders

-- utility

pure : a -> (a, Cmd msg)
pure x = (x, Cmd.none)
