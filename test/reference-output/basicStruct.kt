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

class BasicStruct {
@Serializable
data class BasicStruct(
    @get:JsonProperty("field1")
    val field1: Int,
    @get:JsonProperty("field2")
    val field2: String
) : java.io.Serializable
}
