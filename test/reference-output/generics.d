module gotyno_output.generics;

import asdf;
import std.sumtype;
import std.typecons : Nullable;

static import gotyno_output.basic;
static import gotyno_output.has_generic;

struct UsingGenerics
{
    basic.Maybe!(string) field1;
    basic.Either!(string, uint32_t) field2;
}

struct UsingOwnGenerics(T)
{
    basic.Maybe!(T) field1;
}

struct KnownForMovie
{
    string media_type;
    Nullable!(string) poster_path;
    uint32_t id;
    Nullable!(string) title;
    float vote_average;
    Nullable!(string) release_date;
    string overview;
}

struct KnownForShow
{
    string media_type;
    Nullable!(string) poster_path;
    uint32_t id;
    float vote_average;
    string overview;
    Nullable!(string) first_air_date;
    Nullable!(string) name;
}

alias KnownFor = SumType!(KnownForShow, KnownForMovie, string, float);

struct KnownForMovieWithoutTypeTag
{
    Nullable!(string) poster_path;
    uint32_t id;
    Nullable!(string) title;
    float vote_average;
    Nullable!(string) release_date;
    string overview;
}

struct KnownForShowWithoutTypeTag
{
    Nullable!(string) poster_path;
    uint32_t id;
    float vote_average;
    string overview;
    Nullable!(string) first_air_date;
    Nullable!(string) name;
}

struct _MovieStartingWithLowercase
{
    KnownForMovieWithoutTypeTag data;
}

struct _TvStartingWithLowercase
{
    KnownForShowWithoutTypeTag data;
}

struct KnownForEmbedded
{
    alias Type = SumType!(_MovieStartingWithLowercase, _TvStartingWithLowercase);
    Type data;
    alias data this;

    static foreach (T; Type.Types)
        this(T v) @safe pure nothrow @nogc { data = v; }

    import asdf;
    SerdeException deserializeFromAsdf(Asdf asdfData)
    {
        string tag;
        if (auto e = asdfData["media_type"].deserializeValue(tag)) return e;

        final switch (tag)
        {
            case "movieStartingWithLowercase": {
                KnownForMovieWithoutTypeTag v = void;
                if (auto e = asdfData.deserializeValue(v)) return e;
                data = _MovieStartingWithLowercase(v);
                return null;
            }

            case "tvStartingWithLowercase": {
                KnownForShowWithoutTypeTag v = void;
                if (auto e = asdfData.deserializeValue(v)) return e;
                data = _TvStartingWithLowercase(v);
                return null;
            }

            default: return new SerdeException("Unknown tag: " ~ tag);
        }
    }
}

struct _Movie
{
    KnownForMovieWithoutTypeTag data;
}

struct _Tv
{
    KnownForShowWithoutTypeTag data;
}

struct KnownForEmbeddedWithUpperCase
{
    alias Type = SumType!(_Movie, _Tv);
    Type data;
    alias data this;

    static foreach (T; Type.Types)
        this(T v) @safe pure nothrow @nogc { data = v; }

    import asdf;
    SerdeException deserializeFromAsdf(Asdf asdfData)
    {
        string tag;
        if (auto e = asdfData["media_type"].deserializeValue(tag)) return e;

        final switch (tag)
        {
            case "Movie": {
                KnownForMovieWithoutTypeTag v = void;
                if (auto e = asdfData.deserializeValue(v)) return e;
                data = _Movie(v);
                return null;
            }

            case "Tv": {
                KnownForShowWithoutTypeTag v = void;
                if (auto e = asdfData.deserializeValue(v)) return e;
                data = _Tv(v);
                return null;
            }

            default: return new SerdeException("Unknown tag: " ~ tag);
        }
    }
}
