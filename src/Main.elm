port module Main exposing (..)

import Browser
import Cmd.Extra exposing (withCmd, withNoCmd)
import HashAlgorithm exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Maths exposing (Integer)
import TriMaybe exposing (..)


type alias JsonValue =
    Decode.Value


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }



-- PORTS


port askHash : ( String, String ) -> Cmd msg


port onHash : (String -> msg) -> Sub msg


port askHmac : { message : String, key : String, algorithm : String } -> Cmd msg


port onHmac : (String -> msg) -> Sub msg


port askHashFileContent : JsonValue -> Cmd msg


port onHashFileContent : (String -> msg) -> Sub msg


port askIsPrime : String -> Cmd msg


port onPrimalityResolved : (Bool -> msg) -> Sub msg



-- STRUCTURE


type alias Model =
    PrimeModel (HashModel (HmacModel (PowModel {})))


type alias HashModel r =
    { r
        | hashAlgorithm : Maybe HashAlgorithm
        , hashInput : Maybe String
        , computedHash : Maybe String
        , hashInputType : HashInputType
    }


type HashInputType
    = TextInput
    | FileInput


type alias HmacModel r =
    { r
        | hmacAlgorithm : Maybe HashAlgorithm
        , hmacMessage : String
        , hmacKey : String
        , computedHmac : Maybe String
    }


type alias PowModel r =
    { r
        | powBase : TriMaybe Integer
        , powExponent : TriMaybe Integer
        , powModulo : TriMaybe Integer
    }


type alias PrimeModel r =
    { r
        | primeInput : TriMaybe Integer
        , primeResult : Maybe Bool
    }


type HashMsg
    = SwitchHashAlgorithm String
    | PickHashInput HashInputType
    | HashInputTyped String
    | HashComputed String
    | HashFileSelected JsonValue
    | HashFileContentFound String


type HmacMsg
    = SwitchHmacAlgorithm String
    | HmacInputTyped String
    | HmacKeyTyped String
    | HmacComputed String


type PowMsg
    = PowBaseTyped String
    | PowExponentTyped String
    | PowModuloTyped String


type PrimeMsg
    = PrimeInputTyped String
    | PrimalityDecided Bool


type Msg
    = HashMsg HashMsg
    | HmacMsg HmacMsg
    | PowMsg PowMsg
    | PrimeMsg PrimeMsg


init : () -> ( Model, Cmd Msg )
init _ =
    { hashAlgorithm = Nothing
    , hashInput = Nothing
    , hashInputType = TextInput
    , computedHash = Nothing
    , hmacAlgorithm = Nothing
    , hmacMessage = ""
    , hmacKey = ""
    , computedHmac = Nothing
    , powBase = Empty
    , powExponent = Empty
    , powModulo = Empty
    , primeInput = Empty
    , primeResult = Nothing
    }
        |> withNoCmd



-- UPDATE


updateHashModel : HashMsg -> HashModel r -> ( HashModel r, Cmd Msg )
updateHashModel message model =
    case message of
        SwitchHashAlgorithm name ->
            case parseAlgorithm name of
                Nothing ->
                    { model | hashAlgorithm = Nothing, computedHash = Nothing } |> withNoCmd

                Just algo ->
                    { model | hashAlgorithm = Just algo, computedHash = Nothing }
                        |> withCmd
                            (model.hashInput
                                |> Maybe.map (\input -> askHash ( name, input ))
                                |> Maybe.withDefault Cmd.none
                            )

        HashInputTyped str ->
            ( { model | hashInput = Just str }
            , model.hashAlgorithm
                |> Maybe.map (\algo -> askHash ( algorithmName algo, str ))
                |> Maybe.withDefault Cmd.none
            )

        HashComputed str ->
            { model | computedHash = Just str } |> withNoCmd

        PickHashInput type_ ->
            { model | hashInputType = type_, computedHash = Nothing, hashInput = Nothing } |> withNoCmd

        HashFileSelected event ->
            ( { model | hashInputType = FileInput }
            , askHashFileContent event
            )

        HashFileContentFound content ->
            ( { model | hashInput = Just content }
            , model.hashAlgorithm
                |> Maybe.map (\algo -> askHash ( algorithmName algo, content ))
                |> Maybe.withDefault Cmd.none
            )


updateHmacModel : HmacMsg -> HmacModel r -> ( HmacModel r, Cmd Msg )
updateHmacModel message model =
    case message of
        SwitchHmacAlgorithm name ->
            case parseAlgorithm name of
                Nothing ->
                    { model | computedHmac = Nothing } |> withNoCmd

                Just algo ->
                    ( { model | hmacAlgorithm = Just algo, computedHmac = Nothing }
                    , askHmac { algorithm = name, message = model.hmacMessage, key = model.hmacKey }
                    )

        HmacInputTyped str ->
            ( { model | hmacMessage = str }
            , case model.hmacAlgorithm of
                Nothing ->
                    Cmd.none

                Just algo ->
                    askHmac { algorithm = algorithmName algo, message = str, key = model.hmacKey }
            )

        HmacKeyTyped str ->
            ( { model | hmacKey = str }
            , case model.hmacAlgorithm of
                Nothing ->
                    Cmd.none

                Just algo ->
                    askHmac { algorithm = algorithmName algo, message = model.hmacMessage, key = str }
            )

        HmacComputed str ->
            { model | computedHmac = Just str } |> withNoCmd


convertString : String -> TriMaybe Integer
convertString str =
    if String.isEmpty str then
        Empty

    else
        TriMaybe.fromMaybeInvalid (Maths.stringToInteger str)


updatePowModel : PowMsg -> PowModel r -> PowModel r
updatePowModel msg model =
    let
        convert =
            convertString
    in
    case msg of
        PowBaseTyped str ->
            { model | powBase = convert str }

        PowModuloTyped str ->
            { model | powModulo = convert str }

        PowExponentTyped str ->
            { model | powExponent = convert str }


updatePrimeModel : PrimeMsg -> PrimeModel r -> ( PrimeModel r, Cmd Msg )
updatePrimeModel msg model =
    case msg of
        PrimeInputTyped str ->
            let
                converted =
                    convertString str
            in
            ( { model | primeInput = converted, primeResult = Nothing }
            , case converted of
                Valid x ->
                    askIsPrime (Maths.integerToString x)

                _ ->
                    Cmd.none
            )

        PrimalityDecided result ->
            { model | primeResult = Just result } |> withNoCmd


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HashMsg m ->
            updateHashModel m model

        HmacMsg m ->
            updateHmacModel m model

        PowMsg m ->
            updatePowModel m model |> withNoCmd

        PrimeMsg m ->
            updatePrimeModel m model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onHash (HashComputed >> HashMsg)
        , onHashFileContent (HashFileContentFound >> HashMsg)
        , onHmac (HmacComputed >> HmacMsg)
        , onPrimalityResolved (PrimalityDecided >> PrimeMsg)
        ]



-- VIEWS


algorithmPickView : (String -> msg) -> Html msg
algorithmPickView switch =
    div []
        [ select
            [ name "algo"
            , attribute "aria-label" "Algorithm"
            , onInput switch
            ]
            ([ option [ selected True, disabled True, value "" ] [ text "Algorithm" ] ]
                ++ List.map
                    (\x ->
                        let
                            name =
                                algorithmName x
                        in
                        option [ value name ] [ text name ]
                    )
                    allAlgorithms
            )
        ]


hashView : HashModel r -> Html Msg
hashView model =
    section []
        [ h4 [] [ text "Compute Hash" ]
        , algorithmPickView (SwitchHashAlgorithm >> HashMsg)
        , fieldset []
            [ legend [] [ text "Input type" ]
            , input
                [ type_ "radio"
                , id "hash-text-option"
                , name "hash-input"
                , onClick (TextInput |> PickHashInput |> HashMsg)
                ]
                []
            , label [ for "hash-text-option" ] [ text "Text" ]
            , input
                [ type_ "radio"
                , id "hash-file-option"
                , name "hash-input"
                , onClick (FileInput |> PickHashInput |> HashMsg)
                ]
                []
            , label [ for "hash-file-option" ] [ text "File" ]
            ]
        , case model.hashInputType of
            TextInput ->
                textarea [ placeholder "Input", onInput (HashInputTyped >> HashMsg) ] []

            FileInput ->
                div []
                    [ input
                        [ type_ "file"
                        , on "change" (Decode.map (HashFileSelected >> HashMsg) Decode.value)
                        ]
                        []
                    , small [] [ text "Note: This implementation loads the entire file into memory" ]
                    ]
        , case ( model.hashAlgorithm, model.hashInput, model.computedHash ) of
            ( Just algo, Just input, Just hash ) ->
                div []
                    [ algo |> algorithmName |> text
                    , text " Hash: "
                    , code [] [ text hash ]
                    ]

            _ ->
                div [] []
        ]


hmacView : HmacModel r -> Html Msg
hmacView model =
    section []
        [ h4 [] [ text "HMAC" ]
        , algorithmPickView (SwitchHmacAlgorithm >> HmacMsg)
        , textarea [ placeholder "Message", onInput (HmacInputTyped >> HmacMsg) ] []
        , input [ placeholder "Key", onInput (HmacKeyTyped >> HmacMsg) ] []
        , case model.hmacAlgorithm of
            Nothing ->
                div [] []

            Just algo ->
                div []
                    [ algo |> algorithmName |> text
                    , text " Hash: "
                    , case model.computedHmac of
                        Nothing ->
                            text "...computing"

                        Just str ->
                            code [] [ text str ]
                    ]
        ]


viewNumberInput : String -> TriMaybe Integer -> (String -> msg) -> Html msg
viewNumberInput name value message =
    input
        ([ placeholder name
         , onInput message
         ]
            ++ (if TriMaybe.isInvalid value then
                    [ attribute "aria-invalid" "true" ]

                else
                    []
               )
        )
        []


powView : PowModel r -> Html Msg
powView model =
    section []
        [ h4 [] [ text "Modular Exponentiation" ]
        , viewNumberInput "Base" model.powBase (PowBaseTyped >> PowMsg)
        , viewNumberInput "Exponent" model.powExponent (PowExponentTyped >> PowMsg)
        , viewNumberInput "Modulo" model.powModulo (PowModuloTyped >> PowMsg)
        , case ( model.powBase, model.powModulo, model.powExponent ) of
            ( Valid base, Valid modulo, Valid exponent ) ->
                div []
                    [
                        "Result: "
                        ++
                        (Maths.computePow base exponent modulo
                         |> Maybe.map Maths.integerToString
                         |> Maybe.withDefault "idk")
                        |> text
                    ]

            _ ->
                div [] []
        ]


primeView : PrimeModel r -> Html Msg
primeView model =
    section []
        [ h4 [] [ text "Primality Test" ]
        , small []
            [ text "Note: This uses "
            , a [ href "https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test" ] [ text "Miller Rabin" ]
            , text " so it can't be 100% sure of primality, but it is pretty accurate"
            ]
        , input
            ([ placeholder "p"
             , onInput (PrimeInputTyped >> PrimeMsg)
             ]
                ++ (if isInvalid model.primeInput then
                        [ attribute "aria-invalid" "true" ]

                    else
                        []
                   )
            )
            []
        , case model.primeResult of
            Just prime ->
                if prime then
                    div [] [ text "Looks prime to me" ]

                else
                    div [] [ text "Not prime." ]

            Nothing ->
                div [] []
        ]


view : Model -> Html Msg
view model =
    div []
        [ main_ [ class "container" ]
            [ h1 [] [ text "Cryptool" ]
            , powView model
            , primeView model
            , hashView model
            , hmacView model
            ]
        ]



-- decoders
