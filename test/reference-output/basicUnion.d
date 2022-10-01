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
}
