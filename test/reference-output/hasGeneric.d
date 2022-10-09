module gotyno_output.has_generic;

import asdf;
import std.sumtype;
import std.typecons : Nullable;

static import gotyno_declarations.external;
static import gotyno_declarations.other;

struct _Success(T)
{
    T data;
}

struct _Failure(E)
{
    E data;
}

struct Result(T, E)
{
    alias Type = SumType!(_Success!(T), _Failure!(E));
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
            case "Success": {
                _Success!(T) v = void;
                if (auto e = asdfData.deserializeValue(v)) return e;
                data = v;
                return null;
            }

            case "Failure": {
                _Failure!(E) v = void;
                if (auto e = asdfData.deserializeValue(v)) return e;
                data = v;
                return null;
            }

            default: return new SerdeException("Unknown tag: " ~ tag);
        }
    }
}

struct Holder(T)
{
    T value;
}

struct MaybeHolder(T)
{
    external.Option!(T) value;
    other.Plain otherValue;
}

struct _PlainEvent
{
    other.Plain data;
}

struct _GenericEvent(T)
{
    external.Option!(T) data;
}

struct HasGenericEvent(T)
{
    alias Type = SumType!(_PlainEvent, _GenericEvent!(T));
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
            case "PlainEvent": {
                _PlainEvent v = void;
                if (auto e = asdfData.deserializeValue(v)) return e;
                data = v;
                return null;
            }

            case "GenericEvent": {
                _GenericEvent!(T) v = void;
                if (auto e = asdfData.deserializeValue(v)) return e;
                data = v;
                return null;
            }

            default: return new SerdeException("Unknown tag: " ~ tag);
        }
    }
}
