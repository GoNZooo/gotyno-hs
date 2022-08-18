package org.gotynoOutput

import com.fasterxml.jackson.annotation.*
import com.fasterxml.jackson.module.kotlin.*
import com.fasterxml.jackson.databind.annotation.*
import com.fasterxml.jackson.databind.*
import com.fasterxml.jackson.core.*
import com.fasterxml.jackson.databind.deser.std.*
import java.text.ParseException
import java.math.BigInteger

class Basic {
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

@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.PROPERTY,
    property = "type"
)
sealed class GetSearchesFilter {
    @JsonTypeName("SearchesByQueryLike")
    data class SearchesByQueryLike(val data: String) : GetSearchesFilter(), java.io.Serializable

    @JsonTypeName("SearchesByResultLike")
    data class SearchesByResultLike(val data: String) : GetSearchesFilter(), java.io.Serializable

    @JsonTypeName("NoSearchesFilter")
    @JsonInclude(JsonInclude.Include.NON_DEFAULT)
    object NoSearchesFilter : GetSearchesFilter(), java.io.Serializable
}

data class SearchesParameters(
    @get:JsonProperty("filters")
    val filters: ArrayList<GetSearchesFilter>
) : java.io.Serializable

enum class StillSize(val data: Any) {
    W92("w92"),
    W185("w185"),
    W300("w300"),
    H632("h632"),
    ORIGINAL("original")
}

data class LogInData(
    @get:JsonProperty("username")
    val username: String,
    @get:JsonProperty("password")
    val password: String
) : java.io.Serializable

data class UserId(
    @get:JsonProperty("value")
    val value: String
) : java.io.Serializable

data class Channel(
    @get:JsonProperty("name")
    val name: String,
    @get:JsonProperty("private")
    val private: Boolean
) : java.io.Serializable

data class Email(
    @get:JsonProperty("value")
    val value: String,
    @get:JsonProperty("public")
    val public: Boolean
) : java.io.Serializable

@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.PROPERTY,
    property = "type"
)
sealed class Event {
    @JsonTypeName("LogIn")
    data class LogIn(val data: LogInData) : Event(), java.io.Serializable

    @JsonTypeName("LogOut")
    data class LogOut(val data: UserId) : Event(), java.io.Serializable

    @JsonTypeName("JoinChannels")
    data class JoinChannels(val data: ArrayList<Channel>) : Event(), java.io.Serializable

    @JsonTypeName("SetEmails")
    data class SetEmails(val data: ArrayList<Email>) : Event(), java.io.Serializable
}

@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.PROPERTY,
    property = "type"
)
sealed class Maybe<T> {
    @JsonTypeName("Nothing")
    @JsonInclude(JsonInclude.Include.NON_DEFAULT)
    object Nothing : Maybe<T>(), java.io.Serializable

    @JsonTypeName("Just")
    data class Just<T>(val data: T) : Maybe<T>(), java.io.Serializable
}

@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.PROPERTY,
    property = "type"
)
sealed class Either<L, R> {
    @JsonTypeName("Left")
    data class Left<L, R>(val data: L) : Either<L, R>(), java.io.Serializable

    @JsonTypeName("Right")
    data class Right<L, R>(val data: R) : Either<L, R>(), java.io.Serializable
}

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

@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.PROPERTY,
    property = "type"
)
sealed class EmbeddedEvent {
    @JsonTypeName("EmbeddedLogIn")
    data class EmbeddedLogIn(@JsonValue(true) val data: LogInData) : EmbeddedEvent(), java.io.Serializable

    @JsonTypeName("SystemImploded")
    @JsonInclude(JsonInclude.Include.NON_DEFAULT)
    object SystemImploded : EmbeddedEvent(), java.io.Serializable
}
}