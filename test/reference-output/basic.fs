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
                "emails", GotynoCoders.encodeList (Encode.option Encode.string) value.emails
                "recruiter", Encode.option Recruiter.Encoder value.recruiter
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
                "filters", GotynoCoders.encodeList GetSearchesFilter.Encoder value.filters
            ]

type StillSize =
    | W92
    | W185
    | W300
    | H632
    | Original

    static member Decoder: Decoder<StillSize> =
        GotynoCoders.decodeOneOf Decode.string [|"w92", W92; "w185", W185; "w300", W300; "h632", H632; "original", Original|]

    static member Encoder =
        function
        | W92 -> Encode.string "w92"
        | W185 -> Encode.string "w185"
        | W300 -> Encode.string "w300"
        | H632 -> Encode.string "h632"
        | Original -> Encode.string "original"

type LogInData =
    {
        username: string
        password: string
    }

    static member Decoder: Decoder<LogInData> =
        Decode.object (fun get ->
            {
                username = get.Required.Field "username" Decode.string
                password = get.Required.Field "password" Decode.string
            }
        )

    static member Encoder value =
        Encode.object
            [
                "username", Encode.string value.username
                "password", Encode.string value.password
            ]

type UserId =
    {
        value: string
    }

    static member Decoder: Decoder<UserId> =
        Decode.object (fun get ->
            {
                value = get.Required.Field "value" Decode.string
            }
        )

    static member Encoder value =
        Encode.object
            [
                "value", Encode.string value.value
            ]

type Channel =
    {
        name: string
        ``private``: bool
    }

    static member Decoder: Decoder<Channel> =
        Decode.object (fun get ->
            {
                name = get.Required.Field "name" Decode.string
                ``private`` = get.Required.Field "private" Decode.bool
            }
        )

    static member Encoder value =
        Encode.object
            [
                "name", Encode.string value.name
                "private", Encode.bool value.``private``
            ]

type Email =
    {
        value: string
    }

    static member Decoder: Decoder<Email> =
        Decode.object (fun get ->
            {
                value = get.Required.Field "value" Decode.string
            }
        )

    static member Encoder value =
        Encode.object
            [
                "value", Encode.string value.value
            ]

type Event =
    | LogIn of LogInData
    | LogOut of UserId
    | JoinChannels of list<Channel>
    | SetEmails of list<Email>

    static member LogInDecoder: Decoder<Event> =
        Decode.object (fun get -> LogIn(get.Required.Field "data" LogInData.Decoder))

    static member LogOutDecoder: Decoder<Event> =
        Decode.object (fun get -> LogOut(get.Required.Field "data" UserId.Decoder))

    static member JoinChannelsDecoder: Decoder<Event> =
        Decode.object (fun get -> JoinChannels(get.Required.Field "data" (Decode.list Channel.Decoder)))

    static member SetEmailsDecoder: Decoder<Event> =
        Decode.object (fun get -> SetEmails(get.Required.Field "data" (Decode.list Email.Decoder)))

    static member Decoder: Decoder<Event> =
        GotynoCoders.decodeWithTypeTag
            "type"
            [|
                "LogIn", Event.LogInDecoder
                "LogOut", Event.LogOutDecoder
                "JoinChannels", Event.JoinChannelsDecoder
                "SetEmails", Event.SetEmailsDecoder
            |]

    static member Encoder =
        function
        | LogIn payload ->
            Encode.object [ "type", Encode.string "LogIn"
                            "data", LogInData.Encoder payload ]

        | LogOut payload ->
            Encode.object [ "type", Encode.string "LogOut"
                            "data", UserId.Encoder payload ]

        | JoinChannels payload ->
            Encode.object [ "type", Encode.string "JoinChannels"
                            "data", GotynoCoders.encodeList Channel.Encoder payload ]

        | SetEmails payload ->
            Encode.object [ "type", Encode.string "SetEmails"
                            "data", GotynoCoders.encodeList Email.Encoder payload ]

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

type Either<'l, 'r> =
    | Left of 'l
    | Right of 'r

    static member LeftDecoder decodeL: Decoder<Either<'l, 'r>> =
        Decode.object (fun get -> Left(get.Required.Field "data" decodeL))

    static member RightDecoder decodeR: Decoder<Either<'l, 'r>> =
        Decode.object (fun get -> Right(get.Required.Field "data" decodeR))

    static member Decoder decodeL decodeR: Decoder<Either<'l, 'r>> =
        GotynoCoders.decodeWithTypeTag
            "type"
            [|
                "Left", Either.LeftDecoder decodeL
                "Right", Either.RightDecoder decodeR
            |]

    static member Encoder encodeL encodeR =
        function
        | Left payload ->
            Encode.object [ "type", Encode.string "Left"
                            "data", encodeL payload ]

        | Right payload ->
            Encode.object [ "type", Encode.string "Right"
                            "data", encodeR payload ]

type Person =
    {
        name: string
        age: uint8
        efficiency: float32
        on_vacation: bool
        hobbies: list<string>
        last_fifteen_comments: list<string>
        recruiter: Recruiter
        spouse: Maybe<Person>
    }

    static member Decoder: Decoder<Person> =
        Decode.object (fun get ->
            {
                name = get.Required.Field "name" Decode.string
                age = get.Required.Field "age" Decode.byte
                efficiency = get.Required.Field "efficiency" Decode.float32
                on_vacation = get.Required.Field "on_vacation" Decode.bool
                hobbies = get.Required.Field "hobbies" (Decode.list Decode.string)
                last_fifteen_comments = get.Required.Field "last_fifteen_comments" (Decode.list Decode.string)
                recruiter = get.Required.Field "recruiter" Recruiter.Decoder
                spouse = get.Required.Field "spouse" (Maybe.Decoder Person.Decoder)
            }
        )

    static member Encoder value =
        Encode.object
            [
                "name", Encode.string value.name
                "age", Encode.byte value.age
                "efficiency", Encode.float32 value.efficiency
                "on_vacation", Encode.bool value.on_vacation
                "hobbies", GotynoCoders.encodeList Encode.string value.hobbies
                "last_fifteen_comments", GotynoCoders.encodeList Encode.string value.last_fifteen_comments
                "recruiter", Recruiter.Encoder value.recruiter
                "spouse", (Maybe.Encoder Person.Encoder) value.spouse
            ]