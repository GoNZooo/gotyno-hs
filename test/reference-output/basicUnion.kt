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

class BasicUnion {
@Serializable
data class PayloadStruct(
    @get:JsonProperty("field1")
    val field1: Int
) : java.io.Serializable

@Serializable
@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.EXISTING_PROPERTY,
    property = "type"
)
sealed class BasicUnion : java.io.Serializable {
    @Serializable
    @JsonTypeName("HasStringPayload")
    data class HasStringPayload(val data: String) : BasicUnion(), java.io.Serializable {
        val type = "HasStringPayload"
    }

    @Serializable
    @JsonTypeName("HasPayload")
    data class HasPayload(val data: PayloadStruct) : BasicUnion(), java.io.Serializable {
        val type = "HasPayload"
    }

    @Serializable
    @JsonTypeName("HasNoPayload")
    class HasNoPayload : BasicUnion(), java.io.Serializable {
        val type = "HasNoPayload"

        override fun equals(other: Any?): Boolean {
            return other is HasNoPayload
        }

        override fun hashCode(): Int {
            return 0
        }
    }
}
}
