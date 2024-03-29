module gotyno_output.basic_optional;

import asdf;
import std.sumtype;
import std.typecons : Nullable;

struct HasOptionalString
{
    Nullable!(string) stringField;
    Nullable!(uint32_t[]) optionalArrayField;
    Nullable!(uint32_t)[] arrayOfOptionalField;
}

struct _DoesNot
{
    int32_t data;
}

struct _Does
{
    Nullable!(int32_t) data;
}

struct _HasOptionalStruct
{
    Nullable!(HasOptionalString) data;
}

struct HasOptionalConstructor
{
    alias Type = SumType!(_DoesNot, _Does, _HasOptionalStruct);
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
            case "DoesNot": {
                _DoesNot v = void;
                if (auto e = asdfData.deserializeValue(v)) return e;
                data = v;
                return null;
            }

            case "Does": {
                _Does v = void;
                if (auto e = asdfData.deserializeValue(v)) return e;
                data = v;
                return null;
            }

            case "HasOptionalStruct": {
                _HasOptionalStruct v = void;
                if (auto e = asdfData.deserializeValue(v)) return e;
                data = v;
                return null;
            }

            default: return new SerdeException("Unknown tag: " ~ tag);
        }
    }
}
