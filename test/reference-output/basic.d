module gotyno_output.basic;

import asdf;
import std.sumtype;
import std.typecons : Nullable;

struct Recruiter
{
    string type;
    string Name;
    Nullable!(string)[3] emails;
    Nullable!(Recruiter) recruiter;
    uint64_t created;
}

struct _SearchesByQueryLike
{
    string data;
}

struct _SearchesByResultLike
{
    string data;
}

struct _NoSearchesFilter
{
}

struct GetSearchesFilter
{
    alias Type = SumType!(_SearchesByQueryLike, _SearchesByResultLike, _NoSearchesFilter);
    Type data;
    alias data this;

    static foreach (T; Type.Types)
        this(T v) @safe pure nothrow @nogc { data = v; }

    import asdf;
    SerdeException deserializeFromAsdf(Asdf asdfData)
    {
        string tag;
        if (auto e = asdfData["type"].deserializeValue(tag)) return e;

        final switch (tag)
        {
            case "SearchesByQueryLike": {
                _SearchesByQueryLike v = void;
                if (auto e = asdfData.deserializeValue(v)) return e;
                data = v;
                return null;
            }

            case "SearchesByResultLike": {
                _SearchesByResultLike v = void;
                if (auto e = asdfData.deserializeValue(v)) return e;
                data = v;
                return null;
            }

            case "NoSearchesFilter": {
                data = _NoSearchesFilter();
                return null;
            }

            default: return new SerdeException("Unknown tag: " ~ tag);
        }
    }
}

struct SearchesParameters
{
    GetSearchesFilter[] filters;
}

enum StillSize : string
{
    w92 = "w92",
    w185 = "w185",
    w300 = "w300",
    h632 = "h632",
    original = "original"
}

struct LogInData
{
    string username;
    string password;
}

struct UserId
{
    string value;
}

struct Channel
{
    string name;
    bool _private;
}

struct Email
{
    string value;
    bool _public;
}

struct _LogIn
{
    LogInData data;
}

struct _LogOut
{
    UserId data;
}

struct _JoinChannels
{
    Channel[] data;
}

struct _SetEmails
{
    Email[5] data;
}

struct Event
{
    alias Type = SumType!(_LogIn, _LogOut, _JoinChannels, _SetEmails);
    Type data;
    alias data this;

    static foreach (T; Type.Types)
        this(T v) @safe pure nothrow @nogc { data = v; }

    import asdf;
    SerdeException deserializeFromAsdf(Asdf asdfData)
    {
        string tag;
        if (auto e = asdfData["type"].deserializeValue(tag)) return e;

        final switch (tag)
        {
            case "LogIn": {
                _LogIn v = void;
                if (auto e = asdfData.deserializeValue(v)) return e;
                data = v;
                return null;
            }

            case "LogOut": {
                _LogOut v = void;
                if (auto e = asdfData.deserializeValue(v)) return e;
                data = v;
                return null;
            }

            case "JoinChannels": {
                _JoinChannels v = void;
                if (auto e = asdfData.deserializeValue(v)) return e;
                data = v;
                return null;
            }

            case "SetEmails": {
                _SetEmails v = void;
                if (auto e = asdfData.deserializeValue(v)) return e;
                data = v;
                return null;
            }

            default: return new SerdeException("Unknown tag: " ~ tag);
        }
    }
}

struct _Nothing
{
}

struct _Just(T)
{
    T data;
}

struct Maybe(T)
{
    alias Type = SumType!(_Nothing, _Just!(T));
    Type data;
    alias data this;

    static foreach (T; Type.Types)
        this(T v) @safe pure nothrow @nogc { data = v; }

    import asdf;
    SerdeException deserializeFromAsdf(Asdf asdfData)
    {
        string tag;
        if (auto e = asdfData["type"].deserializeValue(tag)) return e;

        final switch (tag)
        {
            case "Nothing": {
                data = _Nothing();
                return null;
            }

            case "Just": {
                _Just!(T) v = void;
                if (auto e = asdfData.deserializeValue(v)) return e;
                data = v;
                return null;
            }

            default: return new SerdeException("Unknown tag: " ~ tag);
        }
    }
}

struct _Left(L)
{
    L data;
}

struct _Right(R)
{
    R data;
}

struct Either(L, R)
{
    alias Type = SumType!(_Left!(L), _Right!(R));
    Type data;
    alias data this;

    static foreach (T; Type.Types)
        this(T v) @safe pure nothrow @nogc { data = v; }

    import asdf;
    SerdeException deserializeFromAsdf(Asdf asdfData)
    {
        string tag;
        if (auto e = asdfData["type"].deserializeValue(tag)) return e;

        final switch (tag)
        {
            case "Left": {
                _Left!(L) v = void;
                if (auto e = asdfData.deserializeValue(v)) return e;
                data = v;
                return null;
            }

            case "Right": {
                _Right!(R) v = void;
                if (auto e = asdfData.deserializeValue(v)) return e;
                data = v;
                return null;
            }

            default: return new SerdeException("Unknown tag: " ~ tag);
        }
    }
}

struct Person
{
    string name;
    uint8_t age;
    float efficiency;
    bool on_vacation;
    string[] hobbies;
    string[15] last_fifteen_comments;
    Recruiter recruiter;
    Maybe!(Person) spouse;
}

struct _EmbeddedLogIn
{
    LogInData data;
}

struct _SystemImploded
{
}

struct EmbeddedEvent
{
    alias Type = SumType!(_EmbeddedLogIn, _SystemImploded);
    Type data;
    alias data this;

    static foreach (T; Type.Types)
        this(T v) @safe pure nothrow @nogc { data = v; }

    import asdf;
    SerdeException deserializeFromAsdf(Asdf asdfData)
    {
        string tag;
        if (auto e = asdfData["type"].deserializeValue(tag)) return e;

        final switch (tag)
        {
            case "EmbeddedLogIn": {
                LogInData v = void;
                if (auto e = asdfData.deserializeValue(v)) return e;
                data = _EmbeddedLogIn(v);
                return null;
            }

            case "SystemImploded": {
                data = _SystemImploded();
                return null;
            }

            default: return new SerdeException("Unknown tag: " ~ tag);
        }
    }
}
