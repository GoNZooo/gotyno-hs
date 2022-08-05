import com.fasterxml.jackson.annotation.*
import com.fasterxml.jackson.module.kotlin.*
import java.math.BigInteger

data class Recruiter(
    val type: String = "Recruiter",
    val Name: String,
    val emails: Array<String?>,
    val recruiter: Recruiter?,
    val created: BigInteger
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
    val filters: Array<GetSearchesFilter>
)

enum class StillSize {
    w92 = "w92",
    w185 = "w185",
    w300 = "w300",
    h632 = "h632",
    original = "original"
}

data class LogInData(
    val username: String,
    val password: String
)

data class UserId(
    val value: String
)

data class Channel(
    val name: String,
    val private: Boolean
)

data class Email(
    val value: String
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
    data class JoinChannels(val data: Array<Channel>) : Event()

    @JsonTypeName("SetEmails")
    data class SetEmails(val data: Array<Email>) : Event()
}

@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.PROPERTY,
    property = "type"
)
sealed class Maybe<T> {
    @JsonTypeName("Nothing")
    @JsonInclude(JsonInclude.Include.NON_DEFAULT)
    data class Nothing(val data: Unit = Unit) : Maybe<T>()

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
    data class Left<L>(val data: L) : Either<L, R>()

    @JsonTypeName("Right")
    data class Right<R>(val data: R) : Either<L, R>()
}

data class Person(
    val name: String,
    val age: UByte,
    val efficiency: Float,
    val on_vacation: Boolean,
    val hobbies: Array<String>,
    val last_fifteen_comments: Array<String>,
    val recruiter: Recruiter,
    val spouse: Maybe<Person>
)

@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.PROPERTY,
    property = "type"
)
sealed class EmbeddedEvent {
    @JsonTypeName("EmbeddedLogIn")
    data class EmbeddedLogIn(val username: String, val password: String) : EmbeddedEvent()

    @JsonTypeName("SystemImploded")
    @JsonInclude(JsonInclude.Include.NON_DEFAULT)
    data class SystemImploded(val data: Unit = Unit) : EmbeddedEvent()
}