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

class BasicEnumeration {
enum class StringValues(val data: String) : java.io.Serializable {
    @JsonProperty("first") FIRST("first"),
    @JsonProperty("second") SECOND("second"),
    @JsonProperty("Third") THIRD("Third"),
    @JsonProperty("Fourth") FOURTH("Fourth");

    companion object {}
}

enum class IntValues(val data: Int) : java.io.Serializable {
    @JsonProperty(1) FIRST(1),
    @JsonProperty(2) SECOND(2),
    @JsonProperty(3) THIRD(3),
    @JsonProperty(4) FOURTH(4);

    companion object {}
}
}
