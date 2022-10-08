module gotyno_output.generic_struct;

import asdf;
import std.sumtype;
import std.typecons : Nullable;

struct GenericStruct(T)
{
    T field;
}
