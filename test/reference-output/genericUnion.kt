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

class GenericUnion {
@Serializable
@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.EXISTING_PROPERTY,
    property = "type"
)
sealed class GenericUnion<T> : java.io.Serializable {
    @Serializable
    @JsonTypeName("HasTPayload")
    data class HasTPayload<T>(val data: T) : GenericUnion<T>(), java.io.Serializable {
        val type = "HasTPayload"
    }

    @Serializable
    @JsonTypeName("HasNoPayload")
    class HasNoPayload<T> : GenericUnion<T>(), java.io.Serializable {
        val type = "HasNoPayload"

        override fun equals(other: Any?): Boolean {
            return other is HasNoPayload<*>
        }

        override fun hashCode(): Int {
            return 0
        }
    }
}
}
