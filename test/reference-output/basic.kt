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
)

@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.PROPERTY,
    property = "type"
)
sealed class GetSearchesFilter {
    @JsonTypeName("SearchesByQueryLike")
    data class SearchesByQueryLike(val data: String) : GetSearchesFilter()

    @JsonTypeName("SearchesByResultLike")
    data class SearchesByResultLike(val data: String) : GetSearchesFilter()

    @JsonTypeName("NoSearchesFilter")
    @JsonInclude(JsonInclude.Include.NON_DEFAULT)
    data class NoSearchesFilter(val data: Unit = Unit) : GetSearchesFilter()
}

data class SearchesParameters(
    @get:JsonProperty("filters")
    val filters: ArrayList<GetSearchesFilter>
)

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
)

data class UserId(
    @get:JsonProperty("value")
    val value: String
)

data class Channel(
    @get:JsonProperty("name")
    val name: String,
    @get:JsonProperty("private")
    val private: Boolean
)

data class Email(
    @get:JsonProperty("value")
    val value: String,
    @get:JsonProperty("public")
    val public: Boolean
)

@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.PROPERTY,
    property = "type"
)
sealed class Event {
    @JsonTypeName("LogIn")
    data class LogIn(val data: LogInData) : Event()

    @JsonTypeName("LogOut")
    data class LogOut(val data: UserId) : Event()

    @JsonTypeName("JoinChannels")
    data class JoinChannels(val data: ArrayList<Channel>) : Event()

    @JsonTypeName("SetEmails")
    data class SetEmails(val data: ArrayList<Email>) : Event()
}

@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.PROPERTY,
    property = "type"
)
sealed class Maybe<T> {
    @JsonTypeName("Nothing")
    @JsonInclude(JsonInclude.Include.NON_DEFAULT)
    data class Nothing<T>(val data: Unit = Unit) : Maybe<T>()

    @JsonTypeName("Just")
    data class Just<T>(val data: T) : Maybe<T>()
}

@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.PROPERTY,
    property = "type"
)
sealed class Either<L, R> {
    @JsonTypeName("Left")
    data class Left<L, R>(val data: L) : Either<L, R>()

    @JsonTypeName("Right")
    data class Right<L, R>(val data: R) : Either<L, R>()
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
)

@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.PROPERTY,
    property = "type"
)
sealed class EmbeddedEvent {
    @JsonTypeName("EmbeddedLogIn")
    data class EmbeddedLogIn(@JsonValue(true) val data: LogInData) : EmbeddedEvent()

    @JsonTypeName("SystemImploded")
    @JsonInclude(JsonInclude.Include.NON_DEFAULT)
    data class SystemImploded(val data: Unit = Unit) : EmbeddedEvent()
}
}