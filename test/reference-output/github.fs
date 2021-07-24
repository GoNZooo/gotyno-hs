module Github

open Thoth.Json.Net

type UserData =
    {
        Login: string
        Id: uint32
        Avatar_url: string
        Url: string
        Html_url: string
        Followers_url: string
        Gists_url: string
        Repos_url: string
        Site_admin: bool
        Bio: string
        Public_repos: uint32
        Followers: uint32
        Following: uint32
        Created_at: string
        Updated_at: string
        Location: option<string>
        Blog: option<string>
    }

    static member Decoder: Decoder<UserData> =
        Decode.object (fun get ->
            {
                Login = get.Required.Field "login" Decode.string
                Id = get.Required.Field "id" Decode.uint32
                Avatar_url = get.Required.Field "avatar_url" Decode.string
                Url = get.Required.Field "url" Decode.string
                Html_url = get.Required.Field "html_url" Decode.string
                Followers_url = get.Required.Field "followers_url" Decode.string
                Gists_url = get.Required.Field "gists_url" Decode.string
                Repos_url = get.Required.Field "repos_url" Decode.string
                Site_admin = get.Required.Field "site_admin" Decode.bool
                Bio = get.Required.Field "bio" Decode.string
                Public_repos = get.Required.Field "public_repos" Decode.uint32
                Followers = get.Required.Field "followers" Decode.uint32
                Following = get.Required.Field "following" Decode.uint32
                Created_at = get.Required.Field "created_at" Decode.string
                Updated_at = get.Required.Field "updated_at" Decode.string
                Location = get.Optional.Field "location" Decode.string
                Blog = get.Optional.Field "blog" Decode.string
            }
        )

    static member Encoder value =
        Encode.object
            [
                "login", Encode.string value.Login
                "id", Encode.uint32 value.Id
                "avatar_url", Encode.string value.Avatar_url
                "url", Encode.string value.Url
                "html_url", Encode.string value.Html_url
                "followers_url", Encode.string value.Followers_url
                "gists_url", Encode.string value.Gists_url
                "repos_url", Encode.string value.Repos_url
                "site_admin", Encode.bool value.Site_admin
                "bio", Encode.string value.Bio
                "public_repos", Encode.uint32 value.Public_repos
                "followers", Encode.uint32 value.Followers
                "following", Encode.uint32 value.Following
                "created_at", Encode.string value.Created_at
                "updated_at", Encode.string value.Updated_at
                "location", Encode.option Encode.string value.Location
                "blog", Encode.option Encode.string value.Blog
            ]

type OwnerData =
    {
        Id: uint32
        Login: string
        Url: string
        Html_url: string
        Followers_url: string
        Gists_url: string
        Repos_url: string
        Site_admin: bool
    }

    static member Decoder: Decoder<OwnerData> =
        Decode.object (fun get ->
            {
                Id = get.Required.Field "id" Decode.uint32
                Login = get.Required.Field "login" Decode.string
                Url = get.Required.Field "url" Decode.string
                Html_url = get.Required.Field "html_url" Decode.string
                Followers_url = get.Required.Field "followers_url" Decode.string
                Gists_url = get.Required.Field "gists_url" Decode.string
                Repos_url = get.Required.Field "repos_url" Decode.string
                Site_admin = get.Required.Field "site_admin" Decode.bool
            }
        )

    static member Encoder value =
        Encode.object
            [
                "id", Encode.uint32 value.Id
                "login", Encode.string value.Login
                "url", Encode.string value.Url
                "html_url", Encode.string value.Html_url
                "followers_url", Encode.string value.Followers_url
                "gists_url", Encode.string value.Gists_url
                "repos_url", Encode.string value.Repos_url
                "site_admin", Encode.bool value.Site_admin
            ]

type OrganizationData =
    {
        Login: string
        Id: uint32
        Avatar_url: string
        Members_url: option<string>
        Repos_url: string
        Description: option<string>
    }

    static member Decoder: Decoder<OrganizationData> =
        Decode.object (fun get ->
            {
                Login = get.Required.Field "login" Decode.string
                Id = get.Required.Field "id" Decode.uint32
                Avatar_url = get.Required.Field "avatar_url" Decode.string
                Members_url = get.Optional.Field "members_url" Decode.string
                Repos_url = get.Required.Field "repos_url" Decode.string
                Description = get.Optional.Field "description" Decode.string
            }
        )

    static member Encoder value =
        Encode.object
            [
                "login", Encode.string value.Login
                "id", Encode.uint32 value.Id
                "avatar_url", Encode.string value.Avatar_url
                "members_url", Encode.option Encode.string value.Members_url
                "repos_url", Encode.string value.Repos_url
                "description", Encode.option Encode.string value.Description
            ]

type Owner =
    | User of OwnerData
    | Organization of OrganizationData

    static member UserDecoder: Decoder<Owner> =
        Decode.object (fun get ->
            User {
                Id = get.Required.Field "id" Decode.uint32
                Login = get.Required.Field "login" Decode.string
                Url = get.Required.Field "url" Decode.string
                Html_url = get.Required.Field "html_url" Decode.string
                Followers_url = get.Required.Field "followers_url" Decode.string
                Gists_url = get.Required.Field "gists_url" Decode.string
                Repos_url = get.Required.Field "repos_url" Decode.string
                Site_admin = get.Required.Field "site_admin" Decode.bool
            }
        )

    static member OrganizationDecoder: Decoder<Owner> =
        Decode.object (fun get ->
            Organization {
                Login = get.Required.Field "login" Decode.string
                Id = get.Required.Field "id" Decode.uint32
                Avatar_url = get.Required.Field "avatar_url" Decode.string
                Members_url = get.Optional.Field "members_url" Decode.string
                Repos_url = get.Required.Field "repos_url" Decode.string
                Description = get.Optional.Field "description" Decode.string
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
                    "id", Encode.uint32 payload.Id
                    "login", Encode.string payload.Login
                    "url", Encode.string payload.Url
                    "html_url", Encode.string payload.Html_url
                    "followers_url", Encode.string payload.Followers_url
                    "gists_url", Encode.string payload.Gists_url
                    "repos_url", Encode.string payload.Repos_url
                    "site_admin", Encode.bool payload.Site_admin
                ]

        | Organization payload ->
            Encode.object
                [
                    "type", Encode.string "Organization"
                    "login", Encode.string payload.Login
                    "id", Encode.uint32 payload.Id
                    "avatar_url", Encode.string payload.Avatar_url
                    "members_url", Encode.option Encode.string payload.Members_url
                    "repos_url", Encode.string payload.Repos_url
                    "description", Encode.option Encode.string payload.Description
                ]

type Repository =
    {
        Id: uint32
        Name: string
        Full_name: string
        Private: bool
        Fork: bool
        Created_at: string
        Updated_at: string
        Description: option<string>
        Owner: Owner
        Url: string
        Html_url: string
        Language: option<string>
    }

    static member Decoder: Decoder<Repository> =
        Decode.object (fun get ->
            {
                Id = get.Required.Field "id" Decode.uint32
                Name = get.Required.Field "name" Decode.string
                Full_name = get.Required.Field "full_name" Decode.string
                Private = get.Required.Field "private" Decode.bool
                Fork = get.Required.Field "fork" Decode.bool
                Created_at = get.Required.Field "created_at" Decode.string
                Updated_at = get.Required.Field "updated_at" Decode.string
                Description = get.Optional.Field "description" Decode.string
                Owner = get.Required.Field "owner" Owner.Decoder
                Url = get.Required.Field "url" Decode.string
                Html_url = get.Required.Field "html_url" Decode.string
                Language = get.Optional.Field "language" Decode.string
            }
        )

    static member Encoder value =
        Encode.object
            [
                "id", Encode.uint32 value.Id
                "name", Encode.string value.Name
                "full_name", Encode.string value.Full_name
                "private", Encode.bool value.Private
                "fork", Encode.bool value.Fork
                "created_at", Encode.string value.Created_at
                "updated_at", Encode.string value.Updated_at
                "description", Encode.option Encode.string value.Description
                "owner", Owner.Encoder value.Owner
                "url", Encode.string value.Url
                "html_url", Encode.string value.Html_url
                "language", Encode.option Encode.string value.Language
            ]

type Pusher =
    {
        Name: string
        Email: string
    }

    static member Decoder: Decoder<Pusher> =
        Decode.object (fun get ->
            {
                Name = get.Required.Field "name" Decode.string
                Email = get.Required.Field "email" Decode.string
            }
        )

    static member Encoder value =
        Encode.object
            [
                "name", Encode.string value.Name
                "email", Encode.string value.Email
            ]

type Author =
    {
        Name: string
        Email: string
        Username: string
    }

    static member Decoder: Decoder<Author> =
        Decode.object (fun get ->
            {
                Name = get.Required.Field "name" Decode.string
                Email = get.Required.Field "email" Decode.string
                Username = get.Required.Field "username" Decode.string
            }
        )

    static member Encoder value =
        Encode.object
            [
                "name", Encode.string value.Name
                "email", Encode.string value.Email
                "username", Encode.string value.Username
            ]

type Label =
    {
        Id: uint32
        Url: string
        Name: string
        Color: string
        Default: bool
        Description: string
    }

    static member Decoder: Decoder<Label> =
        Decode.object (fun get ->
            {
                Id = get.Required.Field "id" Decode.uint32
                Url = get.Required.Field "url" Decode.string
                Name = get.Required.Field "name" Decode.string
                Color = get.Required.Field "color" Decode.string
                Default = get.Required.Field "default" Decode.bool
                Description = get.Required.Field "description" Decode.string
            }
        )

    static member Encoder value =
        Encode.object
            [
                "id", Encode.uint32 value.Id
                "url", Encode.string value.Url
                "name", Encode.string value.Name
                "color", Encode.string value.Color
                "default", Encode.bool value.Default
                "description", Encode.string value.Description
            ]

type Issue =
    {
        Id: uint32
        Url: string
        Html_url: string
        Repository_url: string
        Number: uint32
        Title: string
        User: UserData
        Labels: list<Label>
        State: string
        Locked: bool
        Assignee: option<UserData>
        Assignees: list<UserData>
        Comments: uint32
        Created_at: string
        Updated_at: string
        Closed_at: option<string>
        Author_association: string
        Body: string
    }

    static member Decoder: Decoder<Issue> =
        Decode.object (fun get ->
            {
                Id = get.Required.Field "id" Decode.uint32
                Url = get.Required.Field "url" Decode.string
                Html_url = get.Required.Field "html_url" Decode.string
                Repository_url = get.Required.Field "repository_url" Decode.string
                Number = get.Required.Field "number" Decode.uint32
                Title = get.Required.Field "title" Decode.string
                User = get.Required.Field "user" UserData.Decoder
                Labels = get.Required.Field "labels" (Decode.list Label.Decoder)
                State = get.Required.Field "state" Decode.string
                Locked = get.Required.Field "locked" Decode.bool
                Assignee = get.Optional.Field "assignee" UserData.Decoder
                Assignees = get.Required.Field "assignees" (Decode.list UserData.Decoder)
                Comments = get.Required.Field "comments" Decode.uint32
                Created_at = get.Required.Field "created_at" Decode.string
                Updated_at = get.Required.Field "updated_at" Decode.string
                Closed_at = get.Optional.Field "closed_at" Decode.string
                Author_association = get.Required.Field "author_association" Decode.string
                Body = get.Required.Field "body" Decode.string
            }
        )

    static member Encoder value =
        Encode.object
            [
                "id", Encode.uint32 value.Id
                "url", Encode.string value.Url
                "html_url", Encode.string value.Html_url
                "repository_url", Encode.string value.Repository_url
                "number", Encode.uint32 value.Number
                "title", Encode.string value.Title
                "user", UserData.Encoder value.User
                "labels", GotynoCoders.encodeList Label.Encoder value.Labels
                "state", Encode.string value.State
                "locked", Encode.bool value.Locked
                "assignee", Encode.option UserData.Encoder value.Assignee
                "assignees", GotynoCoders.encodeList UserData.Encoder value.Assignees
                "comments", Encode.uint32 value.Comments
                "created_at", Encode.string value.Created_at
                "updated_at", Encode.string value.Updated_at
                "closed_at", Encode.option Encode.string value.Closed_at
                "author_association", Encode.string value.Author_association
                "body", Encode.string value.Body
            ]

type Commit =
    {
        Id: string
        Tree_id: string
        Distinct: bool
        Message: string
        Timestamp: string
        Url: string
        Author: Author
        Committer: Author
        Added: list<string>
        Removed: list<string>
        Modified: list<string>
    }

    static member Decoder: Decoder<Commit> =
        Decode.object (fun get ->
            {
                Id = get.Required.Field "id" Decode.string
                Tree_id = get.Required.Field "tree_id" Decode.string
                Distinct = get.Required.Field "distinct" Decode.bool
                Message = get.Required.Field "message" Decode.string
                Timestamp = get.Required.Field "timestamp" Decode.string
                Url = get.Required.Field "url" Decode.string
                Author = get.Required.Field "author" Author.Decoder
                Committer = get.Required.Field "committer" Author.Decoder
                Added = get.Required.Field "added" (Decode.list Decode.string)
                Removed = get.Required.Field "removed" (Decode.list Decode.string)
                Modified = get.Required.Field "modified" (Decode.list Decode.string)
            }
        )

    static member Encoder value =
        Encode.object
            [
                "id", Encode.string value.Id
                "tree_id", Encode.string value.Tree_id
                "distinct", Encode.bool value.Distinct
                "message", Encode.string value.Message
                "timestamp", Encode.string value.Timestamp
                "url", Encode.string value.Url
                "author", Author.Encoder value.Author
                "committer", Author.Encoder value.Committer
                "added", GotynoCoders.encodeList Encode.string value.Added
                "removed", GotynoCoders.encodeList Encode.string value.Removed
                "modified", GotynoCoders.encodeList Encode.string value.Modified
            ]

type PushData =
    {
        Repository: Repository
        Ref: string
        Before: string
        After: string
        Pusher: Pusher
        Organization: OrganizationData
        Sender: UserData
        Created: bool
        Deleted: bool
        Forced: bool
        Compare: string
        Commits: list<Commit>
        Head_commit: Commit
    }

    static member Decoder: Decoder<PushData> =
        Decode.object (fun get ->
            {
                Repository = get.Required.Field "repository" Repository.Decoder
                Ref = get.Required.Field "ref" Decode.string
                Before = get.Required.Field "before" Decode.string
                After = get.Required.Field "after" Decode.string
                Pusher = get.Required.Field "pusher" Pusher.Decoder
                Organization = get.Required.Field "organization" OrganizationData.Decoder
                Sender = get.Required.Field "sender" UserData.Decoder
                Created = get.Required.Field "created" Decode.bool
                Deleted = get.Required.Field "deleted" Decode.bool
                Forced = get.Required.Field "forced" Decode.bool
                Compare = get.Required.Field "compare" Decode.string
                Commits = get.Required.Field "commits" (Decode.list Commit.Decoder)
                Head_commit = get.Required.Field "head_commit" Commit.Decoder
            }
        )

    static member Encoder value =
        Encode.object
            [
                "repository", Repository.Encoder value.Repository
                "ref", Encode.string value.Ref
                "before", Encode.string value.Before
                "after", Encode.string value.After
                "pusher", Pusher.Encoder value.Pusher
                "organization", OrganizationData.Encoder value.Organization
                "sender", UserData.Encoder value.Sender
                "created", Encode.bool value.Created
                "deleted", Encode.bool value.Deleted
                "forced", Encode.bool value.Forced
                "compare", Encode.string value.Compare
                "commits", GotynoCoders.encodeList Commit.Encoder value.Commits
                "head_commit", Commit.Encoder value.Head_commit
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
        Total_count: uint32
        Incomplete_results: bool
        Items: list<Repository>
    }

    static member Decoder: Decoder<RepositorySearchData> =
        Decode.object (fun get ->
            {
                Total_count = get.Required.Field "total_count" Decode.uint32
                Incomplete_results = get.Required.Field "incomplete_results" Decode.bool
                Items = get.Required.Field "items" (Decode.list Repository.Decoder)
            }
        )

    static member Encoder value =
        Encode.object
            [
                "total_count", Encode.uint32 value.Total_count
                "incomplete_results", Encode.bool value.Incomplete_results
                "items", GotynoCoders.encodeList Repository.Encoder value.Items
            ]