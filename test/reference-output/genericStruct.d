module gotyno_output.generic_struct;

import std.sumtype;

struct GenericStruct(T)
{
    T field;
}
