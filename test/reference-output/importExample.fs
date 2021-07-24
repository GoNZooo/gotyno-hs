module ImportExample

open Thoth.Json.Net

type UsesImport =
    {
        Type: string
        Recruiter: Basic.Recruiter
    }

    static member Decoder: Decoder<UsesImport> =
        Decode.object (fun get ->
            {
                Type = get.Required.Field "type" (GotynoCoders.decodeLiteralString "UsesImport")
                Recruiter = get.Required.Field "recruiter" Basic.Recruiter.Decoder
            }
        )

    static member Encoder value =
        Encode.object
            [
                "type", Encode.string "UsesImport"
                "recruiter", Basic.Recruiter.Encoder value.Recruiter
            ]

type HoldsSomething<'t> =
    {
        HoldingField: 't
    }

    static member Decoder decodeT: Decoder<HoldsSomething<'t>> =
        Decode.object (fun get ->
            {
                HoldingField = get.Required.Field "holdingField" decodeT
            }
        )

    static member Encoder encodeT value =
        Encode.object
            [
                "holdingField", encodeT value.HoldingField
            ]

type StructureUsingImport =
    {
        Event: Basic.Event
    }

    static member Decoder: Decoder<StructureUsingImport> =
        Decode.object (fun get ->
            {
                Event = get.Required.Field "event" Basic.Event.Decoder
            }
        )

    static member Encoder value =
        Encode.object
            [
                "event", Basic.Event.Encoder value.Event
            ]

type UnionUsingImport =
    | CoolEvent of Basic.Event
    | Other of Basic.Person

    static member CoolEventDecoder: Decoder<UnionUsingImport> =
        Decode.object (fun get -> CoolEvent(get.Required.Field "data" Basic.Event.Decoder))

    static member OtherDecoder: Decoder<UnionUsingImport> =
        Decode.object (fun get -> Other(get.Required.Field "data" Basic.Person.Decoder))

    static member Decoder: Decoder<UnionUsingImport> =
        GotynoCoders.decodeWithTypeTag
            "type"
            [|
                "CoolEvent", UnionUsingImport.CoolEventDecoder
                "Other", UnionUsingImport.OtherDecoder
            |]

    static member Encoder =
        function
        | CoolEvent payload ->
            Encode.object [ "type", Encode.string "CoolEvent"
                            "data", Basic.Event.Encoder payload ]

        | Other payload ->
            Encode.object [ "type", Encode.string "Other"
                            "data", Basic.Person.Encoder payload ]

type AllConcrete =
    {
        Field: HoldsSomething<Basic.Either<Basic.Maybe<StructureUsingImport>, UnionUsingImport>>
    }

    static member Decoder: Decoder<AllConcrete> =
        Decode.object (fun get ->
            {
                Field = get.Required.Field "field" (HoldsSomething.Decoder (Basic.Either.Decoder (Basic.Maybe.Decoder StructureUsingImport.Decoder) UnionUsingImport.Decoder))
            }
        )

    static member Encoder value =
        Encode.object
            [
                "field", (HoldsSomething.Encoder (Basic.Either.Encoder (Basic.Maybe.Encoder StructureUsingImport.Encoder) UnionUsingImport.Encoder)) value.Field
            ]