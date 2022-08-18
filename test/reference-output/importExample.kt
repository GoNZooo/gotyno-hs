package org.gotynoOutput

import com.fasterxml.jackson.annotation.*
import com.fasterxml.jackson.module.kotlin.*
import com.fasterxml.jackson.databind.annotation.*
import com.fasterxml.jackson.databind.*
import com.fasterxml.jackson.core.*
import com.fasterxml.jackson.databind.deser.std.*
import java.text.ParseException
import java.math.BigInteger

import org.gotynoOutput.Basic

class ImportExample {
data class UsesImport(
    @get:JsonProperty("recruiter")
    val recruiter: Basic.Recruiter,
    @get:JsonProperty("type")
    val type: String = "UsesImport"
) : java.io.Serializable

data class HoldsSomething<T>(
    @get:JsonProperty("holdingField")
    val holdingField: T
) : java.io.Serializable

data class StructureUsingImport(
    @get:JsonProperty("event")
    val event: Basic.Event
) : java.io.Serializable

@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.PROPERTY,
    property = "type"
)
sealed class UnionUsingImport {
    @JsonTypeName("CoolEvent")
    data class CoolEvent(val data: Basic.Event) : UnionUsingImport(), java.io.Serializable

    @JsonTypeName("Other")
    data class Other(val data: Basic.Person) : UnionUsingImport(), java.io.Serializable
}

data class AllConcrete(
    @get:JsonProperty("field")
    val field: HoldsSomething<Basic.Either<Basic.Maybe<StructureUsingImport>, UnionUsingImport>>
) : java.io.Serializable
}