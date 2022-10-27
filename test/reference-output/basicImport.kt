package org.gotynoOutput

import com.fasterxml.jackson.annotation.*
import com.fasterxml.jackson.module.kotlin.*
import com.fasterxml.jackson.databind.annotation.*
import com.fasterxml.jackson.databind.*
import com.fasterxml.jackson.core.*
import com.fasterxml.jackson.databind.deser.std.*
import java.text.ParseException
import java.math.BigInteger
import kotlinx.serialization.Serializable
import org.gotynoDeclarations.BigIntegerSerializer

import org.gotynoOutput.BasicStruct

class BasicImport {
@Serializable
data class StructUsingImport(
    @get:JsonProperty("field")
    val field: BasicStruct.BasicStruct
) : java.io.Serializable

@Serializable
@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.EXISTING_PROPERTY,
    property = "type"
)
sealed class UnionUsingImport : java.io.Serializable {
    @Serializable
    @JsonTypeName("ConstructorWithPayload")
    data class ConstructorWithPayload(val data: BasicStruct.BasicStruct) : UnionUsingImport(), java.io.Serializable {
        val type = "ConstructorWithPayload"
    }
}
}
