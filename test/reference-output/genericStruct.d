module gotyno_output.generic_struct;

static import asdf;
import std.sumtype;

struct GenericStruct(T)
{
    T field;
}
