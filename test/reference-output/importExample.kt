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

import org.gotynoOutput.Basic

class ImportExample {
@Serializable
data class UsesImport(
    @get:JsonProperty("recruiter")
    val recruiter: Basic.Recruiter,
    @get:JsonProperty("type")
    val type: String = "UsesImport"
) : java.io.Serializable {
    companion object {
        fun create(recruiter: Basic.Recruiter, type: String = "UsesImport"): UsesImport {
            return UsesImport(type = type, recruiter = recruiter)
        }
    }
}

@Serializable
data class HoldsSomething<T>(
    @get:JsonProperty("holdingField")
    val holdingField: T
) : java.io.Serializable {
    companion object {
        fun <T> create(holdingField: T): HoldsSomething<T> {
            return HoldsSomething(holdingField = holdingField)
        }
    }
}

@Serializable
data class StructureUsingImport(
    @get:JsonProperty("event")
    val event: Basic.Event
) : java.io.Serializable {
    companion object {
        fun create(event: Basic.Event): StructureUsingImport {
            return StructureUsingImport(event = event)
        }
    }
}

@Serializable
@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.EXISTING_PROPERTY,
    property = "type"
)
sealed class UnionUsingImport : java.io.Serializable {
    @Serializable
    @JsonTypeName("CoolEvent")
    data class CoolEvent(val data: Basic.Event) : UnionUsingImport(), java.io.Serializable {
        val type = "CoolEvent"
    }

    @Serializable
    @JsonTypeName("Other")
    data class Other(val data: Basic.Person) : UnionUsingImport(), java.io.Serializable {
        val type = "Other"
    }
}

@Serializable
data class AllConcrete(
    @get:JsonProperty("field")
    val field: HoldsSomething<Basic.Either<Basic.Maybe<StructureUsingImport>, UnionUsingImport>>
) : java.io.Serializable {
    companion object {
        fun create(field: HoldsSomething<Basic.Either<Basic.Maybe<StructureUsingImport>, UnionUsingImport>>): AllConcrete {
            return AllConcrete(field = field)
        }
    }
}
}
