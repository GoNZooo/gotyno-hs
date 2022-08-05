package org.gotynoOutput

import com.fasterxml.jackson.annotation.*
import com.fasterxml.jackson.module.kotlin.*
import java.math.BigInteger

import org.gotynoOutput.Basic
import org.gotynoOutput.HasGeneric

class Generics {
data class UsingGenerics(
    val field1: Basic.Maybe<String>,
    val field2: Basic.Either<String, UInt>
)

data class UsingOwnGenerics<T>(
    val field1: Basic.Maybe<T>
)

data class KnownForMovie(
    val poster_path: String?,
    val id: UInt,
    val title: String?,
    val vote_average: Float,
    val release_date: String?,
    val overview: String,
    val media_type: String = "movie"
)

data class KnownForShow(
    val poster_path: String?,
    val id: UInt,
    val vote_average: Float,
    val overview: String,
    val first_air_date: String?,
    val name: String?,
    val media_type: String = "tv"
)

// @TODO: add support for untagged unions

data class KnownForMovieWithoutTypeTag(
    val poster_path: String?,
    val id: UInt,
    val title: String?,
    val vote_average: Float,
    val release_date: String?,
    val overview: String
)

data class KnownForShowWithoutTypeTag(
    val poster_path: String?,
    val id: UInt,
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