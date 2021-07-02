module HasGeneric

open Thoth.Json.Net

type Maybe<'t> =
    | Nothing
    | Just of 't

    static member NothingDecoder: Decoder<Maybe<'t>> =
        Decode.succeed Nothing

    static member JustDecoder decodeT: Decoder<Maybe<'t>> =
        Decode.object (fun get -> Just(get.Required.Field "data" decodeT))

    static member Decoder decodeT: Decoder<Maybe<'t>> =
        GotynoCoders.decodeWithTypeTag
            "type"
            [|
                "Nothing", Maybe.NothingDecoder
                "Just", Maybe.JustDecoder decodeT
            |]

    static member Encoder encodeT =
        function
        | Nothing ->
            Encode.object [ "type", Encode.string "Nothing" ]

        | Just payload ->
            Encode.object [ "type", Encode.string "Just"
                            "data", encodeT payload ]

type Result<'t, 'e> =
    | Success of 't
    | Failure of 'e

    static member SuccessDecoder decodeT: Decoder<Result<'t, 'e>> =
        Decode.object (fun get -> Success(get.Required.Field "data" decodeT))

    static member FailureDecoder decodeE: Decoder<Result<'t, 'e>> =
        Decode.object (fun get -> Failure(get.Required.Field "data" decodeE))

    static member Decoder decodeT decodeE: Decoder<Result<'t, 'e>> =
        GotynoCoders.decodeWithTypeTag
            "type"
            [|
                "Success", Result.SuccessDecoder decodeT
                "Failure", Result.FailureDecoder decodeE
            |]

    static member Encoder encodeT encodeE =
        function
        | Success payload ->
            Encode.object [ "type", Encode.string "Success"
                            "data", encodeT payload ]

        | Failure payload ->
            Encode.object [ "type", Encode.string "Failure"
                            "data", encodeE payload ]

type Holder<'t> =
    {
        value: 't
    }

    static member Decoder decodeT: Decoder<Holder<'t>> =
        Decode.object (fun get ->
            {
                value = get.Required.Field "value" decodeT
            }
        )

    static member Encoder encodeT value =
        Encode.object
            [
                "value", encodeT value.value
            ]

type MaybeHolder<'t> =
    {
        value: option<'t>
    }

    static member Decoder decodeT: Decoder<MaybeHolder<'t>> =
        Decode.object (fun get ->
            {
                value = get.Optional.Field "value" decodeT
            }
        )

    static member Encoder encodeT value =
        Encode.object
            [
                "value", Encode.option encodeT value.value
            ]