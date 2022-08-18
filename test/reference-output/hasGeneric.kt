package org.gotynoOutput

import com.fasterxml.jackson.annotation.*
import com.fasterxml.jackson.module.kotlin.*
import com.fasterxml.jackson.databind.annotation.*
import com.fasterxml.jackson.databind.*
import com.fasterxml.jackson.core.*
import com.fasterxml.jackson.databind.deser.std.*
import java.text.ParseException
import java.math.BigInteger

import org.gotynoDeclarations.*

class HasGeneric {
@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.PROPERTY,
    property = "type"
)
sealed class Result<T, E> {
    @JsonTypeName("Success")
    data class Success<T, E>(val data: T) : Result<T, E>(), java.io.Serializable

    @JsonTypeName("Failure")
    data class Failure<T, E>(val data: E) : Result<T, E>(), java.io.Serializable
}

data class Holder<T>(
    @get:JsonProperty("value")
    val value: T
) : java.io.Serializable

data class MaybeHolder<T>(
    @get:JsonProperty("value")
    val value: External_Option<T>,
    @get:JsonProperty("otherValue")
    val otherValue: Other_Plain
) : java.io.Serializable

@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.PROPERTY,
    property = "type"
)
sealed class HasGenericEvent<T> {
    @JsonTypeName("PlainEvent")
    data class PlainEvent<T>(val data: Other_Plain) : HasGenericEvent<T>(), java.io.Serializable

    @JsonTypeName("GenericEvent")
    data class GenericEvent<T>(val data: External_Option<T>) : HasGenericEvent<T>(), java.io.Serializable
}
}