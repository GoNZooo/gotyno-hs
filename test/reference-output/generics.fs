module Generics

open Thoth.Json.Net

type UsingGenerics =
    {
        field1: Basic.Maybe<string>
        field2: Basic.Either<string, uint32>
    }

    static member Decoder: Decoder<UsingGenerics> =
        Decode.object (fun get ->
            {
                field1 = get.Required.Field "field1" (Basic.Maybe.Decoder Decode.string)
                field2 = get.Required.Field "field2" (Basic.Either.Decoder Decode.string Decode.uint32)
            }
        )

    static member Encoder value =
        Encode.object
            [
                "field1", (Basic.Maybe.Encoder Encode.string) value.field1
                "field2", (Basic.Either.Encoder Encode.string Encode.uint32) value.field2
            ]

type UsingOwnGenerics<'t> =
    {
        field1: Basic.Maybe<'t>
    }

    static member Decoder decodeT: Decoder<UsingOwnGenerics<'t>> =
        Decode.object (fun get ->
            {
                field1 = get.Required.Field "field1" (Basic.Maybe.Decoder decodeT)
            }
        )

    static member Encoder encodeT value =
        Encode.object
            [
                "field1", (Basic.Maybe.Encoder encodeT) value.field1
            ]

type KnownForMovie =
    {
        media_type: string
        poster_path: option<string>
        id: uint32
        title: option<string>
        vote_average: float32
        release_date: option<string>
        overview: string
    }

    static member Decoder: Decoder<KnownForMovie> =
        Decode.object (fun get ->
            {
                media_type = get.Required.Field "media_type" (GotynoCoders.decodeLiteralString "movie")
                poster_path = get.Optional.Field "poster_path" Decode.string
                id = get.Required.Field "id" Decode.uint32
                title = get.Optional.Field "title" Decode.string
                vote_average = get.Required.Field "vote_average" Decode.float32
                release_date = get.Optional.Field "release_date" Decode.string
                overview = get.Required.Field "overview" Decode.string
            }
        )

    static member Encoder value =
        Encode.object
            [
                "media_type", Encode.string "movie"
                "poster_path", Encode.option Encode.string value.poster_path
                "id", Encode.uint32 value.id
                "title", Encode.option Encode.string value.title
                "vote_average", Encode.float32 value.vote_average
                "release_date", Encode.option Encode.string value.release_date
                "overview", Encode.string value.overview
            ]

type KnownForShow =
    {
        media_type: string
        poster_path: option<string>
        id: uint32
        vote_average: float32
        overview: string
        first_air_date: option<string>
        name: option<string>
    }

    static member Decoder: Decoder<KnownForShow> =
        Decode.object (fun get ->
            {
                media_type = get.Required.Field "media_type" (GotynoCoders.decodeLiteralString "tv")
                poster_path = get.Optional.Field "poster_path" Decode.string
                id = get.Required.Field "id" Decode.uint32
                vote_average = get.Required.Field "vote_average" Decode.float32
                overview = get.Required.Field "overview" Decode.string
                first_air_date = get.Optional.Field "first_air_date" Decode.string
                name = get.Optional.Field "name" Decode.string
            }
        )

    static member Encoder value =
        Encode.object
            [
                "media_type", Encode.string "tv"
                "poster_path", Encode.option Encode.string value.poster_path
                "id", Encode.uint32 value.id
                "vote_average", Encode.float32 value.vote_average
                "overview", Encode.string value.overview
                "first_air_date", Encode.option Encode.string value.first_air_date
                "name", Encode.option Encode.string value.name
            ]

type KnownFor =
    | KnownForKnownForShow of KnownForShow
    | KnownForKnownForMovie of KnownForMovie
    | KnownForString of string
    | KnownForF32 of float32

    static member KnownForKnownForShowDecoder: Decoder<KnownFor> =
        Decode.map KnownForKnownForShow KnownForShow.Decoder

    static member KnownForKnownForMovieDecoder: Decoder<KnownFor> =
        Decode.map KnownForKnownForMovie KnownForMovie.Decoder

    static member KnownForStringDecoder: Decoder<KnownFor> =
        Decode.map KnownForString Decode.string

    static member KnownForF32Decoder: Decoder<KnownFor> =
        Decode.map KnownForF32 Decode.float32

    static member Decoder: Decoder<KnownFor> =
        Decode.oneOf
            [
                KnownFor.KnownForKnownForShowDecoder
                KnownFor.KnownForKnownForMovieDecoder
                KnownFor.KnownForStringDecoder
                KnownFor.KnownForF32Decoder
            ]

    static member Encoder =
        function
        | KnownForKnownForShow payload ->
            KnownForShow.Encoder payload

        | KnownForKnownForMovie payload ->
            KnownForMovie.Encoder payload

        | KnownForString payload ->
            Encode.string payload

        | KnownForF32 payload ->
            Encode.float32 payload

type KnownForMovieWithoutTypeTag =
    {
        poster_path: option<string>
        id: uint32
        title: option<string>
        vote_average: float32
        release_date: option<string>
        overview: string
    }

    static member Decoder: Decoder<KnownForMovieWithoutTypeTag> =
        Decode.object (fun get ->
            {
                poster_path = get.Optional.Field "poster_path" Decode.string
                id = get.Required.Field "id" Decode.uint32
                title = get.Optional.Field "title" Decode.string
                vote_average = get.Required.Field "vote_average" Decode.float32
                release_date = get.Optional.Field "release_date" Decode.string
                overview = get.Required.Field "overview" Decode.string
            }
        )

    static member Encoder value =
        Encode.object
            [
                "poster_path", Encode.option Encode.string value.poster_path
                "id", Encode.uint32 value.id
                "title", Encode.option Encode.string value.title
                "vote_average", Encode.float32 value.vote_average
                "release_date", Encode.option Encode.string value.release_date
                "overview", Encode.string value.overview
            ]

type KnownForShowWithoutTypeTag =
    {
        poster_path: option<string>
        id: uint32
        vote_average: float32
        overview: string
        first_air_date: option<string>
        name: option<string>
    }

    static member Decoder: Decoder<KnownForShowWithoutTypeTag> =
        Decode.object (fun get ->
            {
                poster_path = get.Optional.Field "poster_path" Decode.string
                id = get.Required.Field "id" Decode.uint32
                vote_average = get.Required.Field "vote_average" Decode.float32
                overview = get.Required.Field "overview" Decode.string
                first_air_date = get.Optional.Field "first_air_date" Decode.string
                name = get.Optional.Field "name" Decode.string
            }
        )

    static member Encoder value =
        Encode.object
            [
                "poster_path", Encode.option Encode.string value.poster_path
                "id", Encode.uint32 value.id
                "vote_average", Encode.float32 value.vote_average
                "overview", Encode.string value.overview
                "first_air_date", Encode.option Encode.string value.first_air_date
                "name", Encode.option Encode.string value.name
            ]

type KnownForEmbedded =
    | MovieStartingWithLowercase of KnownForMovieWithoutTypeTag
    | TvStartingWithLowercase of KnownForShowWithoutTypeTag

    static member MovieStartingWithLowercaseDecoder: Decoder<KnownForEmbedded> =
        Decode.object (fun get ->
            MovieStartingWithLowercase {
                poster_path = get.Optional.Field "poster_path" Decode.string
                id = get.Required.Field "id" Decode.uint32
                title = get.Optional.Field "title" Decode.string
                vote_average = get.Required.Field "vote_average" Decode.float32
                release_date = get.Optional.Field "release_date" Decode.string
                overview = get.Required.Field "overview" Decode.string
            }
        )

    static member TvStartingWithLowercaseDecoder: Decoder<KnownForEmbedded> =
        Decode.object (fun get ->
            TvStartingWithLowercase {
                poster_path = get.Optional.Field "poster_path" Decode.string
                id = get.Required.Field "id" Decode.uint32
                vote_average = get.Required.Field "vote_average" Decode.float32
                overview = get.Required.Field "overview" Decode.string
                first_air_date = get.Optional.Field "first_air_date" Decode.string
                name = get.Optional.Field "name" Decode.string
            }
        )

    static member Decoder: Decoder<KnownForEmbedded> =
        GotynoCoders.decodeWithTypeTag
            "media_type"
            [|
                "movieStartingWithLowercase", KnownForEmbedded.MovieStartingWithLowercaseDecoder
                "tvStartingWithLowercase", KnownForEmbedded.TvStartingWithLowercaseDecoder
            |]

    static member Encoder =
        function
        | MovieStartingWithLowercase payload ->
            Encode.object
                [
                    "media_type", Encode.string "movieStartingWithLowercase"
                    "poster_path", Encode.option Encode.string payload.poster_path
                    "id", Encode.uint32 payload.id
                    "title", Encode.option Encode.string payload.title
                    "vote_average", Encode.float32 payload.vote_average
                    "release_date", Encode.option Encode.string payload.release_date
                    "overview", Encode.string payload.overview
                ]

        | TvStartingWithLowercase payload ->
            Encode.object
                [
                    "media_type", Encode.string "tvStartingWithLowercase"
                    "poster_path", Encode.option Encode.string payload.poster_path
                    "id", Encode.uint32 payload.id
                    "vote_average", Encode.float32 payload.vote_average
                    "overview", Encode.string payload.overview
                    "first_air_date", Encode.option Encode.string payload.first_air_date
                    "name", Encode.option Encode.string payload.name
                ]

type KnownForEmbeddedWithUpperCase =
    | Movie of KnownForMovieWithoutTypeTag
    | Tv of KnownForShowWithoutTypeTag

    static member MovieDecoder: Decoder<KnownForEmbeddedWithUpperCase> =
        Decode.object (fun get ->
            Movie {
                poster_path = get.Optional.Field "poster_path" Decode.string
                id = get.Required.Field "id" Decode.uint32
                title = get.Optional.Field "title" Decode.string
                vote_average = get.Required.Field "vote_average" Decode.float32
                release_date = get.Optional.Field "release_date" Decode.string
                overview = get.Required.Field "overview" Decode.string
            }
        )

    static member TvDecoder: Decoder<KnownForEmbeddedWithUpperCase> =
        Decode.object (fun get ->
            Tv {
                poster_path = get.Optional.Field "poster_path" Decode.string
                id = get.Required.Field "id" Decode.uint32
                vote_average = get.Required.Field "vote_average" Decode.float32
                overview = get.Required.Field "overview" Decode.string
                first_air_date = get.Optional.Field "first_air_date" Decode.string
                name = get.Optional.Field "name" Decode.string
            }
        )

    static member Decoder: Decoder<KnownForEmbeddedWithUpperCase> =
        GotynoCoders.decodeWithTypeTag
            "media_type"
            [|
                "Movie", KnownForEmbeddedWithUpperCase.MovieDecoder
                "Tv", KnownForEmbeddedWithUpperCase.TvDecoder
            |]

    static member Encoder =
        function
        | Movie payload ->
            Encode.object
                [
                    "media_type", Encode.string "Movie"
                    "poster_path", Encode.option Encode.string payload.poster_path
                    "id", Encode.uint32 payload.id
                    "title", Encode.option Encode.string payload.title
                    "vote_average", Encode.float32 payload.vote_average
                    "release_date", Encode.option Encode.string payload.release_date
                    "overview", Encode.string payload.overview
                ]

        | Tv payload ->
            Encode.object
                [
                    "media_type", Encode.string "Tv"
                    "poster_path", Encode.option Encode.string payload.poster_path
                    "id", Encode.uint32 payload.id
                    "vote_average", Encode.float32 payload.vote_average
                    "overview", Encode.string payload.overview
                    "first_air_date", Encode.option Encode.string payload.first_air_date
                    "name", Encode.option Encode.string payload.name
                ]