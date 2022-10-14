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

class BasicOptional {
@Serializable
data class HasOptionalString(
    @get:JsonProperty("stringField")
    val stringField: String?,
    @get:JsonProperty("optionalArrayField")
    val optionalArrayField: ArrayList<Int>?,
    @get:JsonProperty("arrayOfOptionalField")
    val arrayOfOptionalField: ArrayList<Int?>
) : java.io.Serializable

@Serializable
@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.EXISTING_PROPERTY,
    property = "type"
)
sealed class HasOptionalConstructor : java.io.Serializable {
    @Serializable
    @JsonTypeName("DoesNot")
    data class DoesNot(val data: Int) : HasOptionalConstructor(), java.io.Serializable {
        val type = "DoesNot"
    }

    @Serializable
    @JsonTypeName("Does")
    data class Does(val data: Int?) : HasOptionalConstructor(), java.io.Serializable {
        val type = "Does"
    }

    @Serializable
    @JsonTypeName("HasOptionalStruct")
    data class HasOptionalStruct(val data: HasOptionalString?) : HasOptionalConstructor(), java.io.Serializable {
        val type = "HasOptionalStruct"
    }
}
}
