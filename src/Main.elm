
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


type HashMsg =
    SwitchHashAlgorithm String |
    HashInputTyped String |
    HashComputed String

type HmacMsg =
    SwitchHmacAlgorithm String |
    HmacInputTyped String |
    HmacKeyTyped String |
    HmacComputed String

type PowMsg =
    PowBaseTyped String |
    PowExponentTyped String |
    PowModuloTyped String

type Msg = HashMsg HashMsg | HmacMsg HmacMsg | PowMsg PowMsg



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


updateHashModel : HashMsg -> HashModel -> (HashModel, Cmd Msg)
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

updateHmacModel : HmacMsg -> HmacModel -> (HmacModel, Cmd Msg)
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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        HashMsg m ->
            updateHashModel m model.hash
            |> Tuple.mapFirst (\hash -> { model | hash = hash})

        HmacMsg m ->
            updateHmacModel m model.hmac
            |> Tuple.mapFirst (\thing -> { model | hmac = thing})

        PowMsg m ->
            model|> pure

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch [onHash (HashComputed >> HashMsg), onHmac (HmacComputed >> HmacMsg)]


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

                algorithmPickView (SwitchHashAlgorithm >> HashMsg),

                input [placeholder "Input", onInput (HashInputTyped >> HashMsg)] [  ],

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

        algorithmPickView (SwitchHmacAlgorithm >> HmacMsg),
        input [placeholder "Message", onInput (HmacInputTyped >> HmacMsg)] [],
        input [placeholder "Key", onInput (HmacKeyTyped >> HmacMsg)] [],

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

        input [ placeholder "Base", onInput (PowBaseTyped >> PowMsg) ] [],
        input [ placeholder "Exponent", onInput (PowExponentTyped >> PowMsg) ] [],
        input [ placeholder "Modulo", onInput (PowModuloTyped >> PowMsg) ] []
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
