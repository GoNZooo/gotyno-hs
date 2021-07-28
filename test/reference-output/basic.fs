module Basic

open Thoth.Json.Net

type Recruiter =
    {
        Type: string
        Name: string
        Emails: list<option<string>>
        Recruiter: option<Recruiter>
        Created: uint64
    }

    static member Decoder: Decoder<Recruiter> =
        Decode.object (fun get ->
            {
                Type = get.Required.Field "type" (GotynoCoders.decodeLiteralString "Recruiter")
                Name = get.Required.Field "Name" Decode.string
                Emails = get.Required.Field "emails" (Decode.list (Decode.option Decode.string))
                Recruiter = get.Optional.Field "recruiter" Recruiter.Decoder
                Created = get.Required.Field "created" Decode.uint64
            }
        )

    static member Encoder value =
        Encode.object
            [
                "type", Encode.string "Recruiter"
                "Name", Encode.string value.Name
                "emails", GotynoCoders.encodeList (Encode.option Encode.string) value.Emails
                "recruiter", Encode.option Recruiter.Encoder value.Recruiter
                "created", Encode.uint64 value.Created
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
        Filters: list<GetSearchesFilter>
    }

    static member Decoder: Decoder<SearchesParameters> =
        Decode.object (fun get ->
            {
                Filters = get.Required.Field "filters" (Decode.list GetSearchesFilter.Decoder)
            }
        )

    static member Encoder value =
        Encode.object
            [
                "filters", GotynoCoders.encodeList GetSearchesFilter.Encoder value.Filters
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
        Username: string
        Password: string
    }

    static member Decoder: Decoder<LogInData> =
        Decode.object (fun get ->
            {
                Username = get.Required.Field "username" Decode.string
                Password = get.Required.Field "password" Decode.string
            }
        )

    static member Encoder value =
        Encode.object
            [
                "username", Encode.string value.Username
                "password", Encode.string value.Password
            ]

type UserId =
    {
        Value: string
    }

    static member Decoder: Decoder<UserId> =
        Decode.object (fun get ->
            {
                Value = get.Required.Field "value" Decode.string
            }
        )

    static member Encoder value =
        Encode.object
            [
                "value", Encode.string value.Value
            ]

type Channel =
    {
        Name: string
        Private: bool
    }

    static member Decoder: Decoder<Channel> =
        Decode.object (fun get ->
            {
                Name = get.Required.Field "name" Decode.string
                Private = get.Required.Field "private" Decode.bool
            }
        )

    static member Encoder value =
        Encode.object
            [
                "name", Encode.string value.Name
                "private", Encode.bool value.Private
            ]

type Email =
    {
        Value: string
    }

    static member Decoder: Decoder<Email> =
        Decode.object (fun get ->
            {
                Value = get.Required.Field "value" Decode.string
            }
        )

    static member Encoder value =
        Encode.object
            [
                "value", Encode.string value.Value
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
        Name: string
        Age: uint8
        Efficiency: float32
        On_vacation: bool
        Hobbies: list<string>
        Last_fifteen_comments: list<string>
        Recruiter: Recruiter
        Spouse: Maybe<Person>
    }

    static member Decoder: Decoder<Person> =
        Decode.object (fun get ->
            {
                Name = get.Required.Field "name" Decode.string
                Age = get.Required.Field "age" Decode.byte
                Efficiency = get.Required.Field "efficiency" Decode.float32
                On_vacation = get.Required.Field "on_vacation" Decode.bool
                Hobbies = get.Required.Field "hobbies" (Decode.list Decode.string)
                Last_fifteen_comments = get.Required.Field "last_fifteen_comments" (Decode.list Decode.string)
                Recruiter = get.Required.Field "recruiter" Recruiter.Decoder
                Spouse = get.Required.Field "spouse" (Maybe.Decoder Person.Decoder)
            }
        )

    static member Encoder value =
        Encode.object
            [
                "name", Encode.string value.Name
                "age", Encode.byte value.Age
                "efficiency", Encode.float32 value.Efficiency
                "on_vacation", Encode.bool value.On_vacation
                "hobbies", GotynoCoders.encodeList Encode.string value.Hobbies
                "last_fifteen_comments", GotynoCoders.encodeList Encode.string value.Last_fifteen_comments
                "recruiter", Recruiter.Encoder value.Recruiter
                "spouse", (Maybe.Encoder Person.Encoder) value.Spouse
            ]

type EmbeddedEvent =
    | EmbeddedLogIn of LogInData
    | SystemImploded

    static member EmbeddedLogInDecoder: Decoder<EmbeddedEvent> =
        Decode.object (fun get ->
            EmbeddedLogIn {
                Username = get.Required.Field "username" Decode.string
                Password = get.Required.Field "password" Decode.string
            }
        )

    static member SystemImplodedDecoder: Decoder<EmbeddedEvent> =
        Decode.object (fun get -> SystemImploded)

    static member Decoder: Decoder<EmbeddedEvent> =
        GotynoCoders.decodeWithTypeTag
            "type"
            [|
                "EmbeddedLogIn", EmbeddedEvent.EmbeddedLogInDecoder
                "SystemImploded", EmbeddedEvent.SystemImplodedDecoder
            |]

    static member Encoder =
        function
        | EmbeddedLogIn payload ->
            Encode.object
                [
                    "type", Encode.string "EmbeddedLogIn"
                    "username", Encode.string payload.Username
                    "password", Encode.string payload.Password
                ]

        | SystemImploded payload ->
            Encode.object
                [
                    "type", Encode.string "SystemImploded"
                ]