module gotyno_output.generic_union;

import std.sumtype;

struct HasTPayloadData(T)
{
    T data;
}

struct HasNoPayloadData
{
}

template GenericUnion(T)
{
    alias Type = SumType!(HasTPayloadData!(T), HasNoPayloadData);
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
                HasTPayloadData!(T) v = void;
                if (auto e = asdfData.deserializeValue(v)) return e;
                data = v;
                return null;
            }

            case "HasNoPayload": {
                data = HasNoPayloadData();
                return null;
            }

            default: return new SerdeException("Unknown tag: " ~ tag);
        }
    }
}
