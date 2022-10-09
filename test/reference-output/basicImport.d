module gotyno_output.basic_import;

import asdf;
import std.sumtype;
import std.typecons : Nullable;

static import gotyno_output.basic_struct;

struct StructUsingImport
{
    basic_struct.BasicStruct field;
}

struct _ConstructorWithPayload
{
    basic_struct.BasicStruct data;
}

struct UnionUsingImport
{
    alias Type = SumType!(_ConstructorWithPayload);
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
            case "ConstructorWithPayload": {
                _ConstructorWithPayload v = void;
                if (auto e = asdfData.deserializeValue(v)) return e;
                data = v;
                return null;
            }

            default: return new SerdeException("Unknown tag: " ~ tag);
        }
    }
}
