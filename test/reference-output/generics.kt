package org.gotynoOutput

import com.fasterxml.jackson.annotation.*
import com.fasterxml.jackson.module.kotlin.*
import java.math.BigInteger

import org.gotynoOutput.Basic
import org.gotynoOutput.HasGeneric

class Generics {
data class UsingGenerics(
    val field1: Basic.Maybe<String>,
    val field2: Basic.Either<String, Int>
)

data class UsingOwnGenerics<T>(
    val field1: Basic.Maybe<T>
)

data class KnownForMovie(
    val poster_path: String?,
    val id: Int,
    val title: String?,
    val vote_average: Float,
    val release_date: String?,
    val overview: String,
    val media_type: String = "movie"
)

data class KnownForShow(
    val poster_path: String?,
    val id: Int,
    val vote_average: Float,
    val overview: String,
    val first_air_date: String?,
    val name: String?,
    val media_type: String = "tv"
)

@JsonDeserialize(using = KnownFor.Deserializer::class)
sealed class KnownFor {
    data class _KnownForShow(@JsonValue(true) val data: KnownForShow) : KnownFor()
    data class _KnownForMovie(@JsonValue(true) val data: KnownForMovie) : KnownFor()
    data class _String(@JsonValue(true) val data: String) : KnownFor()
    data class _Float(@JsonValue(true) val data: Float) : KnownFor()

    class Deserializer : StdDeserializer<KnownFor>(KnownFor::class.java) {
        override fun deserialize(p: JsonParser, ctxt: DeserializationContext): KnownFor {
            val text = ctxt.readTree(p).toString()
            val mapper = jacksonObjectMapper()

            try { return _KnownForShow(mapper.readValue(text)) } catch (_: Exception)  {}
            try { return _KnownForMovie(mapper.readValue(text)) } catch (_: Exception)  {}
            try { return _Float(mapper.readValue(text)) } catch (_: Exception)  {}
            try { return _String(mapper.readValue(text)) } catch (_: Exception)  {}
            throw ParseException("Could not deserialize to class 'KnownFor'", 0)
        }
    }
}

data class KnownForMovieWithoutTypeTag(
    val poster_path: String?,
    val id: Int,
    val title: String?,
    val vote_average: Float,
    val release_date: String?,
    val overview: String
)

data class KnownForShowWithoutTypeTag(
    val poster_path: String?,
    val id: Int,
    val vote_average: Float,
    val overview: String,
    val first_air_date: String?,
    val name: String?
)

@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.PROPERTY,
    property = "media_type"
)
sealed class KnownForEmbedded {
    @JsonTypeName("movieStartingWithLowercase")
    data class MovieStartingWithLowercase(@JsonValue(true) val data: KnownForMovieWithoutTypeTag) : KnownForEmbedded()

    @JsonTypeName("tvStartingWithLowercase")
    data class TvStartingWithLowercase(@JsonValue(true) val data: KnownForShowWithoutTypeTag) : KnownForEmbedded()
}

@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.PROPERTY,
    property = "media_type"
)
sealed class KnownForEmbeddedWithUpperCase {
    @JsonTypeName("Movie")
    data class Movie(@JsonValue(true) val data: KnownForMovieWithoutTypeTag) : KnownForEmbeddedWithUpperCase()

    @JsonTypeName("Tv")
    data class Tv(@JsonValue(true) val data: KnownForShowWithoutTypeTag) : KnownForEmbeddedWithUpperCase()
}
}