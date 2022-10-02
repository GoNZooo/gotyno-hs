module gotyno_output.basic_union;

import std.sumtype;

struct PayloadStruct
{
    int32_t field1;
}

struct HasStringPayloadData
{
    string data;
}

struct HasPayloadData
{
    PayloadStruct data;
}

struct HasNoPayloadData
{
}

struct BasicUnion
{
    alias Type = SumType!(HasStringPayloadData, HasPayloadData, HasNoPayloadData);
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
                HasStringPayloadData v = void;
                if (auto e = asdfData.deserializeValue(v)) return e;
                data = v;
                return null;
            }

            case "HasPayload": {
                HasPayloadData v = void;
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
