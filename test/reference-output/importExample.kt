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
    val recruiter: Basic.Recruiter,
    val type: String = "UsesImport"
)

data class HoldsSomething<T>(
    val holdingField: T
)

data class StructureUsingImport(
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
    val field: HoldsSomething<Basic.Either<Basic.Maybe<StructureUsingImport>, UnionUsingImport>>
)
}