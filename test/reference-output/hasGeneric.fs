module HasGeneric

open Thoth.Json.Net

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
        Value: 't
    }

    static member Decoder decodeT: Decoder<Holder<'t>> =
        Decode.object (fun get ->
            {
                Value = get.Required.Field "value" decodeT
            }
        )

    static member Encoder encodeT value =
        Encode.object
            [
                "value", encodeT value.Value
            ]

type MaybeHolder<'t> =
    {
        Value: External.Option<'t>
        OtherValue: Other.Plain
    }

    static member Decoder decodeT: Decoder<MaybeHolder<'t>> =
        Decode.object (fun get ->
            {
                Value = get.Required.Field "value" (External.Option.Decoder decodeT)
                OtherValue = get.Required.Field "otherValue" Other.Plain.Decoder
            }
        )

    static member Encoder encodeT value =
        Encode.object
            [
                "value", (External.Option.Encoder encodeT) value.Value
                "otherValue", Other.Plain.Encoder value.OtherValue
            ]

type HasGenericEvent<'t> =
    | PlainEvent of Other.Plain
    | GenericEvent of External.Option<'t>

    static member PlainEventDecoder: Decoder<HasGenericEvent<'t>> =
        Decode.object (fun get -> PlainEvent(get.Required.Field "data" Other.Plain.Decoder))

    static member GenericEventDecoder decodeT: Decoder<HasGenericEvent<'t>> =
        Decode.object (fun get -> GenericEvent(get.Required.Field "data" (External.Option.Decoder decodeT)))

    static member Decoder decodeT: Decoder<HasGenericEvent<'t>> =
        GotynoCoders.decodeWithTypeTag
            "type"
            [|
                "PlainEvent", HasGenericEvent.PlainEventDecoder
                "GenericEvent", HasGenericEvent.GenericEventDecoder decodeT
            |]

    static member Encoder encodeT =
        function
        | PlainEvent payload ->
            Encode.object [ "type", Encode.string "PlainEvent"
                            "data", Other.Plain.Encoder payload ]

        | GenericEvent payload ->
            Encode.object [ "type", Encode.string "GenericEvent"
                            "data", (External.Option.Encoder encodeT) payload ]