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
    @JsonProperty("recruiter")
    val recruiter: Basic.Recruiter,
    @JsonProperty("type")
    val type: String = "UsesImport"
)

data class HoldsSomething<T>(
    @JsonProperty("holdingField")
    val holdingField: T
)

data class StructureUsingImport(
    @JsonProperty("event")
    val event: Basic.Event
)

@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.PROPERTY,
    property = "type"
)
sealed class UnionUsingImport {
    @JsonTypeName("CoolEvent")
    data class CoolEvent(val data: Basic.Event) : UnionUsingImport()

    @JsonTypeName("Other")
    data class Other(val data: Basic.Person) : UnionUsingImport()
}

data class AllConcrete(
    @JsonProperty("field")
    val field: HoldsSomething<Basic.Either<Basic.Maybe<StructureUsingImport>, UnionUsingImport>>
)
}