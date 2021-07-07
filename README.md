# gotyno-hs

## Options you can pass to the compiler

The help text for the compiler illustrates most of what you can do.

```bash
$ gotyno-hs --help
gotyno - Compile type definitions into encoders/decoders for languages

Usage: gotyno-hs [--ts|--typescript =|-|PATH] [--fs|--fsharp =|-|PATH] 
                 [-w|--watch] [-v|--verbose] GOTYNOFILE
  Compile type definitions into encoders/decoders for languages

Available options:
  --ts,--typescript =|-|PATH
                           Set TypeScript output
  --fs,--fsharp =|-|PATH   Set FSharp output
  -w,--watch               Watch files and recompile automatically
  -v,--verbose             Output info about compilation
  -h,--help                Show this help text
```

### `=|-|PATH`

The output destination for a language can be set to `=` for "same as input file"
`-` for standard output or a path for all output for that language.

## Install

```
stack install
```

## TypeScript example

[basic.gotyno](./examples/basic.gotyno) has an example of some types being
defined and [basic.ts](./examples/basic.ts) is the automatically generated
TypeScript output from this file.

Behind the scenes it's using a validation library I wrote for validating
`unknown` values (for the most part against given interface definitions).

## F# example

[basic.gotyno](./examples/basic.gotyno) has an example of some types being
defined and [basic.fs](./examples/basic.fs) is the automatically generated
F# output from this file.

The F# version uses `Thoth` for JSON decoding, as well as an additional
extension library to it for some custom decoding helpers that I wrote.

Import names (and by extension declaration source names) are automatically
converted to use an initial uppercase letter for F#. Note, however, that
PascalCase is not automatically converted to. If import names are written in
snake_case the output will be Snake_case. For the most part camelCase will work
well for Gotyno file names, since the output will be "CamelCase" for that
particular example.

## The Language

All supported type names are uppercase and type definitions currently are
enforced as such as well.

### Annotations/Types

- `?TypeName` signifies an optional type.
- `*TypeName` signifies a pointer to that type. In languages where pointers are
  hidden from the user this may not be visible in types generated for it.
- `[]TypeName` signifies a sequence of several `TypeName` with a length known at
  run-time, whereas `[N]TypeName` signifies a sequence of several `TypeName`
  with a known and enforced length at compile-time. Some languages may or may
  not have the latter concept and will use only the former for code generation.
- `TypeName<OtherTypeName>` signifies an application of a generic type, such that
  whatever type variable `TypeName` takes in its definition is filled in with
  `OtherTypeName` in this specific instance.
- Conversely, `struct/union TypeName <T>{ ... }` is how one defines a type that
  takes a type parameter. The `<T>` part is seen here to take and provide a type
  `T` for the adjacent scope, hence its position in the syntax.
- The type `"SomeValue"` signifies a literal string of that value and can be
  used very effectively in TypeScript.
- The unsigned integer type is the same, but for integers. It's debatable
  whether this is useful to have.

### Structs

```
struct Recruiter {
    name: String
}

struct Person {
    name: String
    age: U8
    efficiency: F32
    on_vacation: Boolean
    hobbies: []String
    last_fifteen_comments: [15]String
    recruiter: ?Recruiter
    spouse: Maybe<Person>
}

struct Generic <T>{
    field: T
    other_field: OtherType<T>
}
```

### Enums

```
enum Colors {
    red = "FF0000"
    green = "00FF00"
    blue = "0000FF"
}
```

### Unions

#### Tagged

```
union InteractionEvent {
    Click: Coordinates
    KeyPress: KeyCode
    Focus: *Element
}

union Option <T>{
    None
    Some: T
}

union Result<E, T>{
    Error: E
    Ok: T
}
```

#### Untagged

Sometimes a union that carries no extra tags is required, though usually these
will have to be identified less automatically, perhaps via custom tags in their
respective payload:

```
struct SomeType {
    type: "SomeType"
    some_field: F32
    some_other_field: ?String
}

struct SomeOtherType {
    type: "SomeOtherType"
    active: Boolean
    some_other_field: ?String
}

untagged union Possible {
    SomeType
    SomeOtherType
    String
}
```

In TypeScript, for example, the correct type guard and validator for this
untagged union will be generated, and the literal string fields can still be
used for identifying which type one has.

#### Setting the tag key and embedding it in payload structures

The above can also be accomplished by setting the tag key to be embedded in
options passed to the `union` keyword (we can also set which key is used):

```
struct SomeType {
    some_field: F32
    some_other_field: ?String
}

struct SomeOtherType {
    active: Boolean
    some_other_field: ?String
}

union(tag = type_tag, embedded) Possible {
    FirstConstructor: SomeType
    SecondConstructor: SomeOtherType
}
```

This effectively will create a structure where we get the field `type_tag`
embedded in the payload structures (`SomeType` & `SomeOtherType`) with the
values `"FirstConstructor"` and `"SecondConstructor"` respectively.

Note that in order to embed a type key we obviously need the payload (if present)
to be a structure type, otherwise we have no fields to merge the type tag field
into.

Both checks for existence of the referenced payload types and checks that they
are structures are done during compilation.

### Declarations

When a type that is outside of Gotyno files is needed, we can use a declaration
to assert that it exists. It will function much like an import in the generated
code and it's up to the user to supply the required functionality for each
language in the required file:

```
declare external.Option<T>

declare otherExternalModule.Plain

struct HasOption<T> {
    field: Option<T>
    field2: Plain
}
```

Note that the above code will generate an import for each unique source module
(on the left of the '.') and each usage is automatically recognized as a
reference to a declaration. The code output will rely on this module being
present in the same output directory as the generated code.
