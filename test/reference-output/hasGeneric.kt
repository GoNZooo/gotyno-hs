package org.gotynoOutput

import com.fasterxml.jackson.annotation.*
import com.fasterxml.jackson.module.kotlin.*
import java.math.BigInteger

import org.gotynoDeclarations.External
import org.gotynoDeclarations.Other

class HasGeneric {
@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.PROPERTY,
    property = "type"
)
sealed class Result<T, E> {
    @JsonTypeName("Success")
    data class Success<T, E>(val data: T) : Result<T, E>()

    @JsonTypeName("Failure")
    data class Failure<T, E>(val data: E) : Result<T, E>()
}

data class Holder<T>(
    val value: T
)

data class MaybeHolder<T>(
    val value: External.Option<T>,
    val otherValue: Other.Plain
)

@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.PROPERTY,
    property = "type"
)
sealed class HasGenericEvent<T> {
    @JsonTypeName("PlainEvent")
    data class PlainEvent<T>(val data: Other.Plain) : HasGenericEvent<T>()

    @JsonTypeName("GenericEvent")
    data class GenericEvent<T>(val data: External.Option<T>) : HasGenericEvent<T>()
}
}