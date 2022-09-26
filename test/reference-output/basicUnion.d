module gotyno_output.basic_union;

import std.sumtype;

struct PayloadStruct
{
    int32_t field1;
}

struct BasicUnionHasStringPayload
{
    string data;
}

struct BasicUnionHasPayload
{
    PayloadStruct data;
}

struct BasicUnionHasNoPayload
{
}

alias BasicUnion = SumType!(BasicUnionHasStringPayload, BasicUnionHasPayload, BasicUnionHasNoPayload);
