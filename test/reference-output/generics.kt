package org.gotynoOutput

import com.fasterxml.jackson.annotation.*
import com.fasterxml.jackson.module.kotlin.*
import com.fasterxml.jackson.databind.annotation.*
import com.fasterxml.jackson.databind.*
import com.fasterxml.jackson.core.*
import com.fasterxml.jackson.databind.deser.std.*
import java.text.ParseException
import java.math.BigInteger

import org.gotynoOutput.Basic
import org.gotynoOutput.HasGeneric

class Generics {
data class UsingGenerics(
    @JsonProperty("field1")
    val field1: Basic.Maybe<String>,
    @JsonProperty("field2")
    val field2: Basic.Either<String, Int>
)

data class UsingOwnGenerics<T>(
    @JsonProperty("field1")
    val field1: Basic.Maybe<T>
)

data class KnownForMovie(
    @JsonProperty("poster_path")
    val poster_path: String?,
    @JsonProperty("id")
    val id: Int,
    @JsonProperty("title")
    val title: String?,
    @JsonProperty("vote_average")
    val vote_average: Float,
    @JsonProperty("release_date")
    val release_date: String?,
    @JsonProperty("overview")
    val overview: String,
    @JsonProperty("media_type")
    val media_type: String = "movie"
)

data class KnownForShow(
    @JsonProperty("poster_path")
    val poster_path: String?,
    @JsonProperty("id")
    val id: Int,
    @JsonProperty("vote_average")
    val vote_average: Float,
    @JsonProperty("overview")
    val overview: String,
    @JsonProperty("first_air_date")
    val first_air_date: String?,
    @JsonProperty("name")
    val name: String?,
    @JsonProperty("media_type")
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
    @JsonProperty("poster_path")
    val poster_path: String?,
    @JsonProperty("id")
    val id: Int,
    @JsonProperty("title")
    val title: String?,
    @JsonProperty("vote_average")
    val vote_average: Float,
    @JsonProperty("release_date")
    val release_date: String?,
    @JsonProperty("overview")
    val overview: String
)

data class KnownForShowWithoutTypeTag(
    @JsonProperty("poster_path")
    val poster_path: String?,
    @JsonProperty("id")
    val id: Int,
    @JsonProperty("vote_average")
    val vote_average: Float,
    @JsonProperty("overview")
    val overview: String,
    @JsonProperty("first_air_date")
    val first_air_date: String?,
    @JsonProperty("name")
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