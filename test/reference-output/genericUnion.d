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
}
