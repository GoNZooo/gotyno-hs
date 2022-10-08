module gotyno_output.basic_union;

import asdf;
import std.sumtype;
import std.typecons : Nullable;

struct PayloadStruct
{
    int32_t field1;
}

struct _HasStringPayload
{
    string data;
}

struct _HasPayload
{
    PayloadStruct data;
}

struct _HasNoPayload
{
}

struct BasicUnion
{
    alias Type = SumType!(_HasStringPayload, _HasPayload, _HasNoPayload);
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
            case "HasStringPayload": {
                _HasStringPayload v = void;
                if (auto e = asdfData.deserializeValue(v)) return e;
                data = v;
                return null;
            }

            case "HasPayload": {
                _HasPayload v = void;
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
