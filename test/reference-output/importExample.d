module gotyno_output.import_example;

import asdf;
import std.sumtype;
import std.typecons : Nullable;

static import gotyno_output.basic;

struct UsesImport
{
    string type;
    basic.Recruiter recruiter;
}

struct HoldsSomething(T)
{
    T holdingField;
}

struct StructureUsingImport
{
    basic.Event event;
}

struct _CoolEvent
{
    basic.Event data;
}

struct _Other
{
    basic.Person data;
}

struct UnionUsingImport
{
    alias Type = SumType!(_CoolEvent, _Other);
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
            case "CoolEvent": {
                _CoolEvent v = void;
                if (auto e = asdfData.deserializeValue(v)) return e;
                data = v;
                return null;
            }

            case "Other": {
                _Other v = void;
                if (auto e = asdfData.deserializeValue(v)) return e;
                data = v;
                return null;
            }

            default: return new SerdeException("Unknown tag: " ~ tag);
        }
    }
}

struct AllConcrete
{
    HoldsSomething!(basic.Either!(basic.Maybe!(StructureUsingImport), UnionUsingImport)) field;
}
