module Basic

open Thoth.Json.Net

type Recruiter =
    {
        ``type``: string
        name: string
        emails: list<option<string>>
        recruiter: option<Recruiter>
    }

    static member Decoder: Decoder<Recruiter> =
        Decode.object (fun get ->
            {
                ``type`` = get.Required.Field "type" (GotynoCoders.decodeLiteralString "Recruiter")
                name = get.Required.Field "name" Decode.string
                emails = get.Required.Field "emails" (Decode.list (Decode.option Decode.string))
                recruiter = get.Optional.Field "recruiter" Recruiter.Decoder
            }
        )

    static member Encoder value =
        Encode.object
            [
                "type", Encode.string "Recruiter"
                "name", Encode.string value.name
                "emails", (GotynoCoders.encodeList (Encode.option Encode.string) value.emails)
                "recruiter", (Encode.option Recruiter.Encoder value.recruiter)
            ]

type GetSearchesFilter =
    | SearchesByQueryLike of string
    | SearchesByResultLike of string
    | NoSearchesFilter

    static member SearchesByQueryLikeDecoder: Decoder<GetSearchesFilter> =
        Decode.object (fun get -> SearchesByQueryLike(get.Required.Field "data" Decode.string))

    static member SearchesByResultLikeDecoder: Decoder<GetSearchesFilter> =
        Decode.object (fun get -> SearchesByResultLike(get.Required.Field "data" Decode.string))

    static member NoSearchesFilterDecoder: Decoder<GetSearchesFilter> =
        Decode.succeed NoSearchesFilter

    static member Decoder: Decoder<GetSearchesFilter> =
        GotynoCoders.decodeWithTypeTag
            "type"
            [|
                "SearchesByQueryLike", GetSearchesFilter.SearchesByQueryLikeDecoder
                "SearchesByResultLike", GetSearchesFilter.SearchesByResultLikeDecoder
                "NoSearchesFilter", GetSearchesFilter.NoSearchesFilterDecoder
            |]

    static member Encoder =
        function
        | SearchesByQueryLike payload ->
            Encode.object [ "type", Encode.string "SearchesByQueryLike"
                            "data", Encode.string payload ]

        | SearchesByResultLike payload ->
            Encode.object [ "type", Encode.string "SearchesByResultLike"
                            "data", Encode.string payload ]

        | NoSearchesFilter ->
            Encode.object [ "type", Encode.string "NoSearchesFilter" ]

type SearchesParameters =
    {
        filters: list<GetSearchesFilter>
    }

    static member Decoder: Decoder<SearchesParameters> =
        Decode.object (fun get ->
            {
                filters = get.Required.Field "filters" (Decode.list GetSearchesFilter.Decoder)
            }
        )

    static member Encoder value =
        Encode.object
            [
                "filters", (GotynoCoders.encodeList GetSearchesFilter.Encoder value.filters)
            ]