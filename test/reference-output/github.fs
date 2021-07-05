module Github

open Thoth.Json.Net

type UserData =
    {
        login: string
        id: uint32
        avatar_url: string
        url: string
        html_url: string
        followers_url: string
        gists_url: string
        repos_url: string
        site_admin: bool
        bio: string
        public_repos: uint32
        followers: uint32
        following: uint32
        created_at: string
        updated_at: string
        location: option<string>
        blog: option<string>
    }

    static member Decoder: Decoder<UserData> =
        Decode.object (fun get ->
            {
                login = get.Required.Field "login" Decode.string
                id = get.Required.Field "id" Decode.uint32
                avatar_url = get.Required.Field "avatar_url" Decode.string
                url = get.Required.Field "url" Decode.string
                html_url = get.Required.Field "html_url" Decode.string
                followers_url = get.Required.Field "followers_url" Decode.string
                gists_url = get.Required.Field "gists_url" Decode.string
                repos_url = get.Required.Field "repos_url" Decode.string
                site_admin = get.Required.Field "site_admin" Decode.bool
                bio = get.Required.Field "bio" Decode.string
                public_repos = get.Required.Field "public_repos" Decode.uint32
                followers = get.Required.Field "followers" Decode.uint32
                following = get.Required.Field "following" Decode.uint32
                created_at = get.Required.Field "created_at" Decode.string
                updated_at = get.Required.Field "updated_at" Decode.string
                location = get.Optional.Field "location" Decode.string
                blog = get.Optional.Field "blog" Decode.string
            }
        )

    static member Encoder value =
        Encode.object
            [
                "login", Encode.string value.login
                "id", Encode.uint32 value.id
                "avatar_url", Encode.string value.avatar_url
                "url", Encode.string value.url
                "html_url", Encode.string value.html_url
                "followers_url", Encode.string value.followers_url
                "gists_url", Encode.string value.gists_url
                "repos_url", Encode.string value.repos_url
                "site_admin", Encode.bool value.site_admin
                "bio", Encode.string value.bio
                "public_repos", Encode.uint32 value.public_repos
                "followers", Encode.uint32 value.followers
                "following", Encode.uint32 value.following
                "created_at", Encode.string value.created_at
                "updated_at", Encode.string value.updated_at
                "location", Encode.option Encode.string value.location
                "blog", Encode.option Encode.string value.blog
            ]

type OwnerData =
    {
        id: uint32
        login: string
        url: string
        html_url: string
        followers_url: string
        gists_url: string
        repos_url: string
        site_admin: bool
    }

    static member Decoder: Decoder<OwnerData> =
        Decode.object (fun get ->
            {
                id = get.Required.Field "id" Decode.uint32
                login = get.Required.Field "login" Decode.string
                url = get.Required.Field "url" Decode.string
                html_url = get.Required.Field "html_url" Decode.string
                followers_url = get.Required.Field "followers_url" Decode.string
                gists_url = get.Required.Field "gists_url" Decode.string
                repos_url = get.Required.Field "repos_url" Decode.string
                site_admin = get.Required.Field "site_admin" Decode.bool
            }
        )

    static member Encoder value =
        Encode.object
            [
                "id", Encode.uint32 value.id
                "login", Encode.string value.login
                "url", Encode.string value.url
                "html_url", Encode.string value.html_url
                "followers_url", Encode.string value.followers_url
                "gists_url", Encode.string value.gists_url
                "repos_url", Encode.string value.repos_url
                "site_admin", Encode.bool value.site_admin
            ]

type OrganizationData =
    {
        login: string
        id: uint32
        avatar_url: string
        members_url: option<string>
        repos_url: string
        description: option<string>
    }

    static member Decoder: Decoder<OrganizationData> =
        Decode.object (fun get ->
            {
                login = get.Required.Field "login" Decode.string
                id = get.Required.Field "id" Decode.uint32
                avatar_url = get.Required.Field "avatar_url" Decode.string
                members_url = get.Optional.Field "members_url" Decode.string
                repos_url = get.Required.Field "repos_url" Decode.string
                description = get.Optional.Field "description" Decode.string
            }
        )

    static member Encoder value =
        Encode.object
            [
                "login", Encode.string value.login
                "id", Encode.uint32 value.id
                "avatar_url", Encode.string value.avatar_url
                "members_url", Encode.option Encode.string value.members_url
                "repos_url", Encode.string value.repos_url
                "description", Encode.option Encode.string value.description
            ]

type Owner =
    | User of OwnerData
    | Organization of OrganizationData

    static member UserDecoder: Decoder<Owner> =
        Decode.object (fun get ->
            User {
                id = get.Required.Field "id" Decode.uint32
                login = get.Required.Field "login" Decode.string
                url = get.Required.Field "url" Decode.string
                html_url = get.Required.Field "html_url" Decode.string
                followers_url = get.Required.Field "followers_url" Decode.string
                gists_url = get.Required.Field "gists_url" Decode.string
                repos_url = get.Required.Field "repos_url" Decode.string
                site_admin = get.Required.Field "site_admin" Decode.bool
            }
        )

    static member OrganizationDecoder: Decoder<Owner> =
        Decode.object (fun get ->
            Organization {
                login = get.Required.Field "login" Decode.string
                id = get.Required.Field "id" Decode.uint32
                avatar_url = get.Required.Field "avatar_url" Decode.string
                members_url = get.Optional.Field "members_url" Decode.string
                repos_url = get.Required.Field "repos_url" Decode.string
                description = get.Optional.Field "description" Decode.string
            }
        )

    static member Decoder: Decoder<Owner> =
        GotynoCoders.decodeWithTypeTag
            "type"
            [|
                "User", Owner.UserDecoder
                "Organization", Owner.OrganizationDecoder
            |]

    static member Encoder =
        function
        | User payload ->
            Encode.object
                [
                    "type", Encode.string "User"
                    "id", Encode.uint32 payload.id
                    "login", Encode.string payload.login
                    "url", Encode.string payload.url
                    "html_url", Encode.string payload.html_url
                    "followers_url", Encode.string payload.followers_url
                    "gists_url", Encode.string payload.gists_url
                    "repos_url", Encode.string payload.repos_url
                    "site_admin", Encode.bool payload.site_admin
                ]

        | Organization payload ->
            Encode.object
                [
                    "type", Encode.string "Organization"
                    "login", Encode.string payload.login
                    "id", Encode.uint32 payload.id
                    "avatar_url", Encode.string payload.avatar_url
                    "members_url", Encode.option Encode.string payload.members_url
                    "repos_url", Encode.string payload.repos_url
                    "description", Encode.option Encode.string payload.description
                ]

type Repository =
    {
        id: uint32
        name: string
        full_name: string
        ``private``: bool
        fork: bool
        created_at: string
        updated_at: string
        description: option<string>
        owner: Owner
        url: string
        html_url: string
        language: option<string>
    }

    static member Decoder: Decoder<Repository> =
        Decode.object (fun get ->
            {
                id = get.Required.Field "id" Decode.uint32
                name = get.Required.Field "name" Decode.string
                full_name = get.Required.Field "full_name" Decode.string
                ``private`` = get.Required.Field "private" Decode.bool
                fork = get.Required.Field "fork" Decode.bool
                created_at = get.Required.Field "created_at" Decode.string
                updated_at = get.Required.Field "updated_at" Decode.string
                description = get.Optional.Field "description" Decode.string
                owner = get.Required.Field "owner" Owner.Decoder
                url = get.Required.Field "url" Decode.string
                html_url = get.Required.Field "html_url" Decode.string
                language = get.Optional.Field "language" Decode.string
            }
        )

    static member Encoder value =
        Encode.object
            [
                "id", Encode.uint32 value.id
                "name", Encode.string value.name
                "full_name", Encode.string value.full_name
                "private", Encode.bool value.``private``
                "fork", Encode.bool value.fork
                "created_at", Encode.string value.created_at
                "updated_at", Encode.string value.updated_at
                "description", Encode.option Encode.string value.description
                "owner", Owner.Encoder value.owner
                "url", Encode.string value.url
                "html_url", Encode.string value.html_url
                "language", Encode.option Encode.string value.language
            ]

type Pusher =
    {
        name: string
        email: string
    }

    static member Decoder: Decoder<Pusher> =
        Decode.object (fun get ->
            {
                name = get.Required.Field "name" Decode.string
                email = get.Required.Field "email" Decode.string
            }
        )

    static member Encoder value =
        Encode.object
            [
                "name", Encode.string value.name
                "email", Encode.string value.email
            ]

type Author =
    {
        name: string
        email: string
        username: string
    }

    static member Decoder: Decoder<Author> =
        Decode.object (fun get ->
            {
                name = get.Required.Field "name" Decode.string
                email = get.Required.Field "email" Decode.string
                username = get.Required.Field "username" Decode.string
            }
        )

    static member Encoder value =
        Encode.object
            [
                "name", Encode.string value.name
                "email", Encode.string value.email
                "username", Encode.string value.username
            ]

type Label =
    {
        id: uint32
        url: string
        name: string
        color: string
        default: bool
        description: string
    }

    static member Decoder: Decoder<Label> =
        Decode.object (fun get ->
            {
                id = get.Required.Field "id" Decode.uint32
                url = get.Required.Field "url" Decode.string
                name = get.Required.Field "name" Decode.string
                color = get.Required.Field "color" Decode.string
                default = get.Required.Field "default" Decode.bool
                description = get.Required.Field "description" Decode.string
            }
        )

    static member Encoder value =
        Encode.object
            [
                "id", Encode.uint32 value.id
                "url", Encode.string value.url
                "name", Encode.string value.name
                "color", Encode.string value.color
                "default", Encode.bool value.default
                "description", Encode.string value.description
            ]

type Issue =
    {
        id: uint32
        url: string
        html_url: string
        repository_url: string
        number: uint32
        title: string
        user: UserData
        labels: list<Label>
        state: string
        locked: bool
        assignee: option<UserData>
        assignees: list<UserData>
        comments: uint32
        created_at: string
        updated_at: string
        closed_at: option<string>
        author_association: string
        body: string
    }

    static member Decoder: Decoder<Issue> =
        Decode.object (fun get ->
            {
                id = get.Required.Field "id" Decode.uint32
                url = get.Required.Field "url" Decode.string
                html_url = get.Required.Field "html_url" Decode.string
                repository_url = get.Required.Field "repository_url" Decode.string
                number = get.Required.Field "number" Decode.uint32
                title = get.Required.Field "title" Decode.string
                user = get.Required.Field "user" UserData.Decoder
                labels = get.Required.Field "labels" (Decode.list Label.Decoder)
                state = get.Required.Field "state" Decode.string
                locked = get.Required.Field "locked" Decode.bool
                assignee = get.Optional.Field "assignee" UserData.Decoder
                assignees = get.Required.Field "assignees" (Decode.list UserData.Decoder)
                comments = get.Required.Field "comments" Decode.uint32
                created_at = get.Required.Field "created_at" Decode.string
                updated_at = get.Required.Field "updated_at" Decode.string
                closed_at = get.Optional.Field "closed_at" Decode.string
                author_association = get.Required.Field "author_association" Decode.string
                body = get.Required.Field "body" Decode.string
            }
        )

    static member Encoder value =
        Encode.object
            [
                "id", Encode.uint32 value.id
                "url", Encode.string value.url
                "html_url", Encode.string value.html_url
                "repository_url", Encode.string value.repository_url
                "number", Encode.uint32 value.number
                "title", Encode.string value.title
                "user", UserData.Encoder value.user
                "labels", GotynoCoders.encodeList Label.Encoder value.labels
                "state", Encode.string value.state
                "locked", Encode.bool value.locked
                "assignee", Encode.option UserData.Encoder value.assignee
                "assignees", GotynoCoders.encodeList UserData.Encoder value.assignees
                "comments", Encode.uint32 value.comments
                "created_at", Encode.string value.created_at
                "updated_at", Encode.string value.updated_at
                "closed_at", Encode.option Encode.string value.closed_at
                "author_association", Encode.string value.author_association
                "body", Encode.string value.body
            ]

type Commit =
    {
        id: string
        tree_id: string
        distinct: bool
        message: string
        timestamp: string
        url: string
        author: Author
        committer: Author
        added: list<string>
        removed: list<string>
        modified: list<string>
    }

    static member Decoder: Decoder<Commit> =
        Decode.object (fun get ->
            {
                id = get.Required.Field "id" Decode.string
                tree_id = get.Required.Field "tree_id" Decode.string
                distinct = get.Required.Field "distinct" Decode.bool
                message = get.Required.Field "message" Decode.string
                timestamp = get.Required.Field "timestamp" Decode.string
                url = get.Required.Field "url" Decode.string
                author = get.Required.Field "author" Author.Decoder
                committer = get.Required.Field "committer" Author.Decoder
                added = get.Required.Field "added" (Decode.list Decode.string)
                removed = get.Required.Field "removed" (Decode.list Decode.string)
                modified = get.Required.Field "modified" (Decode.list Decode.string)
            }
        )

    static member Encoder value =
        Encode.object
            [
                "id", Encode.string value.id
                "tree_id", Encode.string value.tree_id
                "distinct", Encode.bool value.distinct
                "message", Encode.string value.message
                "timestamp", Encode.string value.timestamp
                "url", Encode.string value.url
                "author", Author.Encoder value.author
                "committer", Author.Encoder value.committer
                "added", GotynoCoders.encodeList Encode.string value.added
                "removed", GotynoCoders.encodeList Encode.string value.removed
                "modified", GotynoCoders.encodeList Encode.string value.modified
            ]

type PushData =
    {
        repository: Repository
        ref: string
        before: string
        after: string
        pusher: Pusher
        organization: OrganizationData
        sender: UserData
        created: bool
        deleted: bool
        forced: bool
        compare: string
        commits: list<Commit>
        head_commit: Commit
    }

    static member Decoder: Decoder<PushData> =
        Decode.object (fun get ->
            {
                repository = get.Required.Field "repository" Repository.Decoder
                ref = get.Required.Field "ref" Decode.string
                before = get.Required.Field "before" Decode.string
                after = get.Required.Field "after" Decode.string
                pusher = get.Required.Field "pusher" Pusher.Decoder
                organization = get.Required.Field "organization" OrganizationData.Decoder
                sender = get.Required.Field "sender" UserData.Decoder
                created = get.Required.Field "created" Decode.bool
                deleted = get.Required.Field "deleted" Decode.bool
                forced = get.Required.Field "forced" Decode.bool
                compare = get.Required.Field "compare" Decode.string
                commits = get.Required.Field "commits" (Decode.list Commit.Decoder)
                head_commit = get.Required.Field "head_commit" Commit.Decoder
            }
        )

    static member Encoder value =
        Encode.object
            [
                "repository", Repository.Encoder value.repository
                "ref", Encode.string value.ref
                "before", Encode.string value.before
                "after", Encode.string value.after
                "pusher", Pusher.Encoder value.pusher
                "organization", OrganizationData.Encoder value.organization
                "sender", UserData.Encoder value.sender
                "created", Encode.bool value.created
                "deleted", Encode.bool value.deleted
                "forced", Encode.bool value.forced
                "compare", Encode.string value.compare
                "commits", GotynoCoders.encodeList Commit.Encoder value.commits
                "head_commit", Commit.Encoder value.head_commit
            ]

type WebhookEvent =
    | Push of PushData

    static member PushDecoder: Decoder<WebhookEvent> =
        Decode.object (fun get -> Push(get.Required.Field "data" PushData.Decoder))

    static member Decoder: Decoder<WebhookEvent> =
        GotynoCoders.decodeWithTypeTag
            "type"
            [|
                "push", WebhookEvent.PushDecoder
            |]

    static member Encoder =
        function
        | Push payload ->
            Encode.object [ "type", Encode.string "push"
                            "data", PushData.Encoder payload ]

type RepositorySearchData =
    {
        total_count: uint32
        incomplete_results: bool
        items: list<Repository>
    }

    static member Decoder: Decoder<RepositorySearchData> =
        Decode.object (fun get ->
            {
                total_count = get.Required.Field "total_count" Decode.uint32
                incomplete_results = get.Required.Field "incomplete_results" Decode.bool
                items = get.Required.Field "items" (Decode.list Repository.Decoder)
            }
        )

    static member Encoder value =
        Encode.object
            [
                "total_count", Encode.uint32 value.total_count
                "incomplete_results", Encode.bool value.incomplete_results
                "items", GotynoCoders.encodeList Repository.Encoder value.items
            ]