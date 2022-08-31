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

class Basic {
@Serializable
data class Recruiter(
    @get:JsonProperty("Name")
    val Name: String,
    @get:JsonProperty("emails")
    val emails: ArrayList<String?>,
    @get:JsonProperty("recruiter")
    val recruiter: Recruiter?,
    @get:JsonProperty("created")
    val created: BigInteger,
    @get:JsonProperty("type")
    val type: String = "Recruiter"
) : java.io.Serializable

@Serializable
@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.EXISTING_PROPERTY,
    property = "type"
)
sealed class GetSearchesFilter : java.io.Serializable {
    @Serializable
    @JsonTypeName("SearchesByQueryLike")
    data class SearchesByQueryLike(val data: String) : GetSearchesFilter(), java.io.Serializable {
        val type = "SearchesByQueryLike"
    }

    @Serializable
    @JsonTypeName("SearchesByResultLike")
    data class SearchesByResultLike(val data: String) : GetSearchesFilter(), java.io.Serializable {
        val type = "SearchesByResultLike"
    }

    @Serializable
    @JsonTypeName("NoSearchesFilter")
    class NoSearchesFilter : GetSearchesFilter(), java.io.Serializable {
        val type = "NoSearchesFilter"

        override fun equals(other: Any?): Boolean {
            return other is NoSearchesFilter
        }

        override fun hashCode(): Int {
            return 0
        }
    }
}

@Serializable
data class SearchesParameters(
    @get:JsonProperty("filters")
    val filters: ArrayList<GetSearchesFilter>
) : java.io.Serializable

enum class StillSize(val data: Any) : java.io.Serializable {
    @JsonProperty("w92") W92("w92"),
    @JsonProperty("w185") W185("w185"),
    @JsonProperty("w300") W300("w300"),
    @JsonProperty("h632") H632("h632"),
    @JsonProperty("original") ORIGINAL("original");

    companion object {}
}

@Serializable
data class LogInData(
    @get:JsonProperty("username")
    val username: String,
    @get:JsonProperty("password")
    val password: String
) : java.io.Serializable

@Serializable
data class UserId(
    @get:JsonProperty("value")
    val value: String
) : java.io.Serializable

@Serializable
data class Channel(
    @get:JsonProperty("name")
    val name: String,
    @get:JsonProperty("private")
    val private: Boolean
) : java.io.Serializable

@Serializable
data class Email(
    @get:JsonProperty("value")
    val value: String,
    @get:JsonProperty("public")
    val public: Boolean
) : java.io.Serializable

@Serializable
@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.EXISTING_PROPERTY,
    property = "type"
)
sealed class Event : java.io.Serializable {
    @Serializable
    @JsonTypeName("LogIn")
    data class LogIn(val data: LogInData) : Event(), java.io.Serializable {
        val type = "LogIn"
    }

    @Serializable
    @JsonTypeName("LogOut")
    data class LogOut(val data: UserId) : Event(), java.io.Serializable {
        val type = "LogOut"
    }

    @Serializable
    @JsonTypeName("JoinChannels")
    data class JoinChannels(val data: ArrayList<Channel>) : Event(), java.io.Serializable {
        val type = "JoinChannels"
    }

    @Serializable
    @JsonTypeName("SetEmails")
    data class SetEmails(val data: ArrayList<Email>) : Event(), java.io.Serializable {
        val type = "SetEmails"
    }
}

@Serializable
@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.EXISTING_PROPERTY,
    property = "type"
)
sealed class Maybe<T> : java.io.Serializable {
    @Serializable
    @JsonTypeName("Nothing")
    class Nothing<T> : Maybe<T>(), java.io.Serializable {
        val type = "Nothing"

        override fun equals(other: Any?): Boolean {
            return other is Nothing<*>
        }

        override fun hashCode(): Int {
            return 0
        }
    }

    @Serializable
    @JsonTypeName("Just")
    data class Just<T>(val data: T) : Maybe<T>(), java.io.Serializable {
        val type = "Just"
    }
}

@Serializable
@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.EXISTING_PROPERTY,
    property = "type"
)
sealed class Either<L, R> : java.io.Serializable {
    @Serializable
    @JsonTypeName("Left")
    data class Left<L, R>(val data: L) : Either<L, R>(), java.io.Serializable {
        val type = "Left"
    }

    @Serializable
    @JsonTypeName("Right")
    data class Right<L, R>(val data: R) : Either<L, R>(), java.io.Serializable {
        val type = "Right"
    }
}

@Serializable
data class Person(
    @get:JsonProperty("name")
    val name: String,
    @get:JsonProperty("age")
    val age: Byte,
    @get:JsonProperty("efficiency")
    val efficiency: Float,
    @get:JsonProperty("on_vacation")
    val on_vacation: Boolean,
    @get:JsonProperty("hobbies")
    val hobbies: ArrayList<String>,
    @get:JsonProperty("last_fifteen_comments")
    val last_fifteen_comments: ArrayList<String>,
    @get:JsonProperty("recruiter")
    val recruiter: Recruiter,
    @get:JsonProperty("spouse")
    val spouse: Maybe<Person>
) : java.io.Serializable

@Serializable
@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.EXISTING_PROPERTY,
    property = "type"
)
sealed class EmbeddedEvent : java.io.Serializable {
    @Serializable
    @JsonTypeName("EmbeddedLogIn")
    data class EmbeddedLogIn(@JsonValue(true) val data: LogInData) : EmbeddedEvent(), java.io.Serializable {
        val type = "EmbeddedLogIn"
    }

    @Serializable
    @JsonTypeName("SystemImploded")
    class SystemImploded : EmbeddedEvent(), java.io.Serializable {
        val type = "SystemImploded"

        override fun equals(other: Any?): Boolean {
            return other is SystemImploded
        }

        override fun hashCode(): Int {
            return 0
        }
    }
}
}