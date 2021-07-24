module Generics

open Thoth.Json.Net

type UsingGenerics =
    {
        Field1: Basic.Maybe<string>
        Field2: Basic.Either<string, uint32>
    }

    static member Decoder: Decoder<UsingGenerics> =
        Decode.object (fun get ->
            {
                Field1 = get.Required.Field "field1" (Basic.Maybe.Decoder Decode.string)
                Field2 = get.Required.Field "field2" (Basic.Either.Decoder Decode.string Decode.uint32)
            }
        )

    static member Encoder value =
        Encode.object
            [
                "field1", (Basic.Maybe.Encoder Encode.string) value.Field1
                "field2", (Basic.Either.Encoder Encode.string Encode.uint32) value.Field2
            ]

type UsingOwnGenerics<'t> =
    {
        Field1: Basic.Maybe<'t>
    }

    static member Decoder decodeT: Decoder<UsingOwnGenerics<'t>> =
        Decode.object (fun get ->
            {
                Field1 = get.Required.Field "field1" (Basic.Maybe.Decoder decodeT)
            }
        )

    static member Encoder encodeT value =
        Encode.object
            [
                "field1", (Basic.Maybe.Encoder encodeT) value.Field1
            ]

type KnownForMovie =
    {
        Media_type: string
        Poster_path: option<string>
        Id: uint32
        Title: option<string>
        Vote_average: float32
        Release_date: option<string>
        Overview: string
    }

    static member Decoder: Decoder<KnownForMovie> =
        Decode.object (fun get ->
            {
                Media_type = get.Required.Field "media_type" (GotynoCoders.decodeLiteralString "movie")
                Poster_path = get.Optional.Field "poster_path" Decode.string
                Id = get.Required.Field "id" Decode.uint32
                Title = get.Optional.Field "title" Decode.string
                Vote_average = get.Required.Field "vote_average" Decode.float32
                Release_date = get.Optional.Field "release_date" Decode.string
                Overview = get.Required.Field "overview" Decode.string
            }
        )

    static member Encoder value =
        Encode.object
            [
                "media_type", Encode.string "movie"
                "poster_path", Encode.option Encode.string value.Poster_path
                "id", Encode.uint32 value.Id
                "title", Encode.option Encode.string value.Title
                "vote_average", Encode.float32 value.Vote_average
                "release_date", Encode.option Encode.string value.Release_date
                "overview", Encode.string value.Overview
            ]

type KnownForShow =
    {
        Media_type: string
        Poster_path: option<string>
        Id: uint32
        Vote_average: float32
        Overview: string
        First_air_date: option<string>
        Name: option<string>
    }

    static member Decoder: Decoder<KnownForShow> =
        Decode.object (fun get ->
            {
                Media_type = get.Required.Field "media_type" (GotynoCoders.decodeLiteralString "tv")
                Poster_path = get.Optional.Field "poster_path" Decode.string
                Id = get.Required.Field "id" Decode.uint32
                Vote_average = get.Required.Field "vote_average" Decode.float32
                Overview = get.Required.Field "overview" Decode.string
                First_air_date = get.Optional.Field "first_air_date" Decode.string
                Name = get.Optional.Field "name" Decode.string
            }
        )

    static member Encoder value =
        Encode.object
            [
                "media_type", Encode.string "tv"
                "poster_path", Encode.option Encode.string value.Poster_path
                "id", Encode.uint32 value.Id
                "vote_average", Encode.float32 value.Vote_average
                "overview", Encode.string value.Overview
                "first_air_date", Encode.option Encode.string value.First_air_date
                "name", Encode.option Encode.string value.Name
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
        Poster_path: option<string>
        Id: uint32
        Title: option<string>
        Vote_average: float32
        Release_date: option<string>
        Overview: string
    }

    static member Decoder: Decoder<KnownForMovieWithoutTypeTag> =
        Decode.object (fun get ->
            {
                Poster_path = get.Optional.Field "poster_path" Decode.string
                Id = get.Required.Field "id" Decode.uint32
                Title = get.Optional.Field "title" Decode.string
                Vote_average = get.Required.Field "vote_average" Decode.float32
                Release_date = get.Optional.Field "release_date" Decode.string
                Overview = get.Required.Field "overview" Decode.string
            }
        )

    static member Encoder value =
        Encode.object
            [
                "poster_path", Encode.option Encode.string value.Poster_path
                "id", Encode.uint32 value.Id
                "title", Encode.option Encode.string value.Title
                "vote_average", Encode.float32 value.Vote_average
                "release_date", Encode.option Encode.string value.Release_date
                "overview", Encode.string value.Overview
            ]

type KnownForShowWithoutTypeTag =
    {
        Poster_path: option<string>
        Id: uint32
        Vote_average: float32
        Overview: string
        First_air_date: option<string>
        Name: option<string>
    }

    static member Decoder: Decoder<KnownForShowWithoutTypeTag> =
        Decode.object (fun get ->
            {
                Poster_path = get.Optional.Field "poster_path" Decode.string
                Id = get.Required.Field "id" Decode.uint32
                Vote_average = get.Required.Field "vote_average" Decode.float32
                Overview = get.Required.Field "overview" Decode.string
                First_air_date = get.Optional.Field "first_air_date" Decode.string
                Name = get.Optional.Field "name" Decode.string
            }
        )

    static member Encoder value =
        Encode.object
            [
                "poster_path", Encode.option Encode.string value.Poster_path
                "id", Encode.uint32 value.Id
                "vote_average", Encode.float32 value.Vote_average
                "overview", Encode.string value.Overview
                "first_air_date", Encode.option Encode.string value.First_air_date
                "name", Encode.option Encode.string value.Name
            ]

type KnownForEmbedded =
    | MovieStartingWithLowercase of KnownForMovieWithoutTypeTag
    | TvStartingWithLowercase of KnownForShowWithoutTypeTag

    static member MovieStartingWithLowercaseDecoder: Decoder<KnownForEmbedded> =
        Decode.object (fun get ->
            MovieStartingWithLowercase {
                Poster_path = get.Optional.Field "poster_path" Decode.string
                Id = get.Required.Field "id" Decode.uint32
                Title = get.Optional.Field "title" Decode.string
                Vote_average = get.Required.Field "vote_average" Decode.float32
                Release_date = get.Optional.Field "release_date" Decode.string
                Overview = get.Required.Field "overview" Decode.string
            }
        )

    static member TvStartingWithLowercaseDecoder: Decoder<KnownForEmbedded> =
        Decode.object (fun get ->
            TvStartingWithLowercase {
                Poster_path = get.Optional.Field "poster_path" Decode.string
                Id = get.Required.Field "id" Decode.uint32
                Vote_average = get.Required.Field "vote_average" Decode.float32
                Overview = get.Required.Field "overview" Decode.string
                First_air_date = get.Optional.Field "first_air_date" Decode.string
                Name = get.Optional.Field "name" Decode.string
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
                    "poster_path", Encode.option Encode.string payload.Poster_path
                    "id", Encode.uint32 payload.Id
                    "title", Encode.option Encode.string payload.Title
                    "vote_average", Encode.float32 payload.Vote_average
                    "release_date", Encode.option Encode.string payload.Release_date
                    "overview", Encode.string payload.Overview
                ]

        | TvStartingWithLowercase payload ->
            Encode.object
                [
                    "media_type", Encode.string "tvStartingWithLowercase"
                    "poster_path", Encode.option Encode.string payload.Poster_path
                    "id", Encode.uint32 payload.Id
                    "vote_average", Encode.float32 payload.Vote_average
                    "overview", Encode.string payload.Overview
                    "first_air_date", Encode.option Encode.string payload.First_air_date
                    "name", Encode.option Encode.string payload.Name
                ]

type KnownForEmbeddedWithUpperCase =
    | Movie of KnownForMovieWithoutTypeTag
    | Tv of KnownForShowWithoutTypeTag

    static member MovieDecoder: Decoder<KnownForEmbeddedWithUpperCase> =
        Decode.object (fun get ->
            Movie {
                Poster_path = get.Optional.Field "poster_path" Decode.string
                Id = get.Required.Field "id" Decode.uint32
                Title = get.Optional.Field "title" Decode.string
                Vote_average = get.Required.Field "vote_average" Decode.float32
                Release_date = get.Optional.Field "release_date" Decode.string
                Overview = get.Required.Field "overview" Decode.string
            }
        )

    static member TvDecoder: Decoder<KnownForEmbeddedWithUpperCase> =
        Decode.object (fun get ->
            Tv {
                Poster_path = get.Optional.Field "poster_path" Decode.string
                Id = get.Required.Field "id" Decode.uint32
                Vote_average = get.Required.Field "vote_average" Decode.float32
                Overview = get.Required.Field "overview" Decode.string
                First_air_date = get.Optional.Field "first_air_date" Decode.string
                Name = get.Optional.Field "name" Decode.string
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
                    "poster_path", Encode.option Encode.string payload.Poster_path
                    "id", Encode.uint32 payload.Id
                    "title", Encode.option Encode.string payload.Title
                    "vote_average", Encode.float32 payload.Vote_average
                    "release_date", Encode.option Encode.string payload.Release_date
                    "overview", Encode.string payload.Overview
                ]

        | Tv payload ->
            Encode.object
                [
                    "media_type", Encode.string "Tv"
                    "poster_path", Encode.option Encode.string payload.Poster_path
                    "id", Encode.uint32 payload.Id
                    "vote_average", Encode.float32 payload.Vote_average
                    "overview", Encode.string payload.Overview
                    "first_air_date", Encode.option Encode.string payload.First_air_date
                    "name", Encode.option Encode.string payload.Name
                ]