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

class GenericStruct {
@Serializable
data class GenericStruct<T>(
    @get:JsonProperty("field")
    val field: T
) : java.io.Serializable {
    companion object {
        fun <T> create(field: T): GenericStruct<T> {
            return GenericStruct(field = field)
        }
    }
}
}
