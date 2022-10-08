module gotyno_output.generic_union;

import asdf;
import std.sumtype;
import std.typecons : Nullable;

struct _HasTPayload(T)
{
    T data;
}

struct _HasNoPayload
{
}

struct GenericUnion(T)
{
    alias Type = SumType!(_HasTPayload!(T), _HasNoPayload);
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
            case "HasTPayload": {
                _HasTPayload!(T) v = void;
                if (auto e = asdfData.deserializeValue(v)) return e;
                data = v;
                return null;
            }

            case "HasNoPayload": {
                data = _HasNoPayload();
                return null;
            }

            default: return new SerdeException("Unknown tag: " ~ tag);
        }
    }
}
