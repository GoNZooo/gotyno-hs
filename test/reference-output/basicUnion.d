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

alias BasicUnion = SumType!(HasStringPayloadData, HasPayloadData, HasNoPayloadData);
