module gotyno_output.basic_enumeration;

import asdf;
import std.sumtype;
import std.typecons : Nullable;

enum StringValues : string
{
    first = "first",
    second = "second",
    third = "Third",
    fourth = "Fourth"
}

enum IntValues : uint32_t
{
    first = 1,
    second = 2,
    third = 3,
    fourth = 4
}
