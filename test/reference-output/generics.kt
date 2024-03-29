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
import org.gotynoOutput.HasGeneric

class Generics {
@Serializable
data class UsingGenerics(
    @get:JsonProperty("field1")
    val field1: Basic.Maybe<String>,
    @get:JsonProperty("field2")
    val field2: Basic.Either<String, Int>
) : java.io.Serializable {
    companion object {
        fun create(field1: Basic.Maybe<String>, field2: Basic.Either<String, Int>): UsingGenerics {
            return UsingGenerics(field1 = field1, field2 = field2)
        }
    }
}

@Serializable
data class UsingOwnGenerics<T>(
    @get:JsonProperty("field1")
    val field1: Basic.Maybe<T>
) : java.io.Serializable {
    companion object {
        fun <T> create(field1: Basic.Maybe<T>): UsingOwnGenerics<T> {
            return UsingOwnGenerics(field1 = field1)
        }
    }
}

@Serializable
data class KnownForMovie(
    @get:JsonProperty("poster_path")
    val poster_path: String?,
    @get:JsonProperty("id")
    val id: Int,
    @get:JsonProperty("title")
    val title: String?,
    @get:JsonProperty("vote_average")
    val vote_average: Float,
    @get:JsonProperty("release_date")
    val release_date: String?,
    @get:JsonProperty("overview")
    val overview: String,
    @get:JsonProperty("media_type")
    val media_type: String = "movie"
) : java.io.Serializable {
    companion object {
        fun create(id: Int, vote_average: Float, overview: String, poster_path: String? = null, title: String? = null, release_date: String? = null, media_type: String = "movie"): KnownForMovie {
            return KnownForMovie(media_type = media_type, poster_path = poster_path, id = id, title = title, vote_average = vote_average, release_date = release_date, overview = overview)
        }
    }
}

@Serializable
data class KnownForShow(
    @get:JsonProperty("poster_path")
    val poster_path: String?,
    @get:JsonProperty("id")
    val id: Int,
    @get:JsonProperty("vote_average")
    val vote_average: Float,
    @get:JsonProperty("overview")
    val overview: String,
    @get:JsonProperty("first_air_date")
    val first_air_date: String?,
    @get:JsonProperty("name")
    val name: String?,
    @get:JsonProperty("media_type")
    val media_type: String = "tv"
) : java.io.Serializable {
    companion object {
        fun create(id: Int, vote_average: Float, overview: String, poster_path: String? = null, first_air_date: String? = null, name: String? = null, media_type: String = "tv"): KnownForShow {
            return KnownForShow(media_type = media_type, poster_path = poster_path, id = id, vote_average = vote_average, overview = overview, first_air_date = first_air_date, name = name)
        }
    }
}

@JsonDeserialize(using = KnownFor.Deserializer::class)
@Serializable
sealed class KnownFor : java.io.Serializable {
    @Serializable
    data class _KnownForShow(@JsonValue(true) val data: KnownForShow) : KnownFor()
    @Serializable
    data class _KnownForMovie(@JsonValue(true) val data: KnownForMovie) : KnownFor()
    @Serializable
    data class _String(@JsonValue(true) val data: String) : KnownFor()
    @Serializable
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

@Serializable
data class KnownForMovieWithoutTypeTag(
    @get:JsonProperty("poster_path")
    val poster_path: String?,
    @get:JsonProperty("id")
    val id: Int,
    @get:JsonProperty("title")
    val title: String?,
    @get:JsonProperty("vote_average")
    val vote_average: Float,
    @get:JsonProperty("release_date")
    val release_date: String?,
    @get:JsonProperty("overview")
    val overview: String
) : java.io.Serializable {
    companion object {
        fun create(id: Int, vote_average: Float, overview: String, poster_path: String? = null, title: String? = null, release_date: String? = null): KnownForMovieWithoutTypeTag {
            return KnownForMovieWithoutTypeTag(poster_path = poster_path, id = id, title = title, vote_average = vote_average, release_date = release_date, overview = overview)
        }
    }
}

@Serializable
data class KnownForShowWithoutTypeTag(
    @get:JsonProperty("poster_path")
    val poster_path: String?,
    @get:JsonProperty("id")
    val id: Int,
    @get:JsonProperty("vote_average")
    val vote_average: Float,
    @get:JsonProperty("overview")
    val overview: String,
    @get:JsonProperty("first_air_date")
    val first_air_date: String?,
    @get:JsonProperty("name")
    val name: String?
) : java.io.Serializable {
    companion object {
        fun create(id: Int, vote_average: Float, overview: String, poster_path: String? = null, first_air_date: String? = null, name: String? = null): KnownForShowWithoutTypeTag {
            return KnownForShowWithoutTypeTag(poster_path = poster_path, id = id, vote_average = vote_average, overview = overview, first_air_date = first_air_date, name = name)
        }
    }
}

@Serializable
@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.EXISTING_PROPERTY,
    property = "media_type"
)
sealed class KnownForEmbedded : java.io.Serializable {
    @Serializable
    @JsonTypeName("movieStartingWithLowercase")
    data class MovieStartingWithLowercase(@JsonValue(true) val data: KnownForMovieWithoutTypeTag) : KnownForEmbedded(), java.io.Serializable {
        val media_type = "movieStartingWithLowercase"
    }

    @Serializable
    @JsonTypeName("tvStartingWithLowercase")
    data class TvStartingWithLowercase(@JsonValue(true) val data: KnownForShowWithoutTypeTag) : KnownForEmbedded(), java.io.Serializable {
        val media_type = "tvStartingWithLowercase"
    }
}

@Serializable
@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.EXISTING_PROPERTY,
    property = "media_type"
)
sealed class KnownForEmbeddedWithUpperCase : java.io.Serializable {
    @Serializable
    @JsonTypeName("Movie")
    data class Movie(@JsonValue(true) val data: KnownForMovieWithoutTypeTag) : KnownForEmbeddedWithUpperCase(), java.io.Serializable {
        val media_type = "Movie"
    }

    @Serializable
    @JsonTypeName("Tv")
    data class Tv(@JsonValue(true) val data: KnownForShowWithoutTypeTag) : KnownForEmbeddedWithUpperCase(), java.io.Serializable {
        val media_type = "Tv"
    }
}
}
