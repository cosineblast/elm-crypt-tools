
port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import HashAlgorithm exposing (..)

import TriMaybe exposing (..)

main = Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }

-- PORTS

port askHash : (String, String) -> Cmd msg
port onHash : (String -> msg) -> Sub msg

port askHmac : { message: String, key: String, algorithm: String } -> Cmd msg
port onHmac : (String -> msg) -> Sub msg

-- STRUCTURE

type alias Model = {
    -- hash
    hashAlgorithm: Maybe HashAlgorithm,
    hashInput: String,
    computedHash: Maybe String,

    -- hmac
    hmacAlgorithm: Maybe HashAlgorithm,
    hmacMessage: String,
    hmacKey: String,
    computedHmac: Maybe String,

    -- pow
    powBase: TriMaybe Int,
    powExponent: TriMaybe Int,
    powModulo: TriMaybe Int
    }

type alias HashModel r = {
        r |
        hashAlgorithm: Maybe HashAlgorithm,
        hashInput: String,
        computedHash: Maybe String
    }

type alias HmacModel r = {
        r |
        hmacAlgorithm: Maybe HashAlgorithm,
        hmacMessage: String,
        hmacKey: String,
        computedHmac: Maybe String
    }

type alias PowModel r = {
        r |
        powBase: TriMaybe Int,
        powExponent: TriMaybe Int,
        powModulo: TriMaybe Int
    }


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
    hashAlgorithm = Nothing,
    hashInput = "",
    computedHash = Nothing,

    hmacAlgorithm = Nothing,
    hmacMessage = "",
    hmacKey = "",
    computedHmac = Nothing,

    powBase = Empty,
    powExponent = Empty,
    powModulo = Empty
    } |> pure


-- UPDATE

updateHashModel : HashMsg -> HashModel r -> (HashModel r, Cmd Msg)
updateHashModel message model =
    case message of
        SwitchHashAlgorithm (name) ->
            case parseAlgorithm name of
                Nothing -> { model | hashAlgorithm = Nothing, computedHash = Nothing } |> pure
                Just algo -> ({ model | hashAlgorithm = Just algo, computedHash = Nothing }, askHash (name, model.hashInput))

        HashInputTyped str -> ({ model | hashInput = str },
            case model.hashAlgorithm of
                Nothing -> Cmd.none
                Just algo -> askHash (algorithmName algo, str))

        HashComputed str -> ({ model | computedHash = Just str }) |> pure

updateHmacModel : HmacMsg -> HmacModel r -> (HmacModel r, Cmd Msg)
updateHmacModel message model =
    case message of
        SwitchHmacAlgorithm name ->
            case parseAlgorithm name of
                Nothing -> { model | computedHmac = Nothing } |> pure
                Just algo -> ({ model | hmacAlgorithm = Just algo, computedHmac = Nothing},
                    askHmac { algorithm = name, message = model.hmacMessage, key = model.hmacKey })
        HmacInputTyped str -> ({ model | hmacMessage = str},
            case model.hmacAlgorithm of
                Nothing -> Cmd.none
                Just algo -> askHmac {algorithm = algorithmName algo, message = str, key = model.hmacKey})

        HmacKeyTyped str -> ({ model | hmacKey = str},
            case model.hmacAlgorithm of
                Nothing -> Cmd.none
                Just algo -> askHmac {algorithm = algorithmName algo, message = model.hmacMessage, key = str})

        HmacComputed str -> { model | computedHmac = Just str } |> pure


updatePowModel : PowMsg -> PowModel r -> PowModel r
updatePowModel msg model =
    let convert = (\str ->
            if String.isEmpty str
                then Empty
                else TriMaybe.fromMaybeInvalid (String.toInt str))

    in case msg of
            PowBaseTyped str -> { model | powBase = convert str }
            PowModuloTyped str -> { model | powModulo = convert str }
            PowExponentTyped str -> { model | powExponent = convert str }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        HashMsg m -> updateHashModel m model
        HmacMsg m -> updateHmacModel m model
        PowMsg m -> updatePowModel m model |> pure

computePow : Int -> Int -> Int -> Int
computePow x y z = (x ^ y) |> remainderBy z

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch [onHash (HashComputed >> HashMsg), onHmac (HmacComputed >> HmacMsg)]

-- VIEWS

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

hashView : HashModel r -> Html Msg
hashView model =
            section [] [

                h4 [] [ text "Compute Hash" ],

                algorithmPickView (SwitchHashAlgorithm >> HashMsg),

                input [placeholder "Input", onInput (HashInputTyped >> HashMsg)] [  ],

                case model.hashAlgorithm of
                    Nothing -> div [] []
                    Just algo -> div []
                        [ algo |> algorithmName |> text,
                          text " Hash: ",
                          case model.computedHash of
                              Nothing -> text "...computing"
                              Just str -> code [] [text str]
                        ]
            ]

hmacView : HmacModel r -> Html Msg
hmacView model =
    section [] [
        h4 [] [text "HMAC"],

        algorithmPickView (SwitchHmacAlgorithm >> HmacMsg),
        input [placeholder "Message", onInput (HmacInputTyped >> HmacMsg)] [],
        input [placeholder "Key", onInput (HmacKeyTyped >> HmacMsg)] [],

        case model.hmacAlgorithm of
            Nothing -> div [] []
            Just algo -> div []
                [ algo |> algorithmName |> text,
                  text " Hash: ",
                  case model.computedHmac of
                      Nothing -> text "...computing"
                      Just str -> code [] [text str]
                ]
        ]


viewNumberInput : String -> TriMaybe Int -> (String -> msg) -> Html msg
viewNumberInput name value message =
    input ([
        placeholder name,
        onInput message
        ] ++ if (TriMaybe.isInvalid value)
            then [attribute "aria-invalid" "true"]
            else []) []

powView : PowModel r -> Html Msg
powView model =
    section [] [
        h4 [] [ text "Modular Exponentiation" ],

        viewNumberInput "Base" model.powBase (PowBaseTyped >> PowMsg),
        viewNumberInput "Exponent" model.powExponent (PowExponentTyped >> PowMsg),
        viewNumberInput "Modulo" model.powModulo (PowModuloTyped >> PowMsg),

        case (model.powBase, model.powModulo, model.powExponent) of
            (Valid base, Valid modulo, Valid exponent) ->
                div [] [ "Result: " ++ String.fromInt (computePow base exponent modulo) |> text ]

            _ -> div [] []
    ]


view : Model -> Html Msg
view model = div []
    [ main_ [ class "container" ]
        [
            h1 [] [ text "Cryptool" ],
            powView model,
            hashView model,
            hmacView model
        ]
    ]

-- decoders

-- utility

pure : a -> (a, Cmd msg)
pure x = (x, Cmd.none)
