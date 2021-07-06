import * as svt from "simple-validation-tools";

import * as basic from "./basic";

import * as hasGeneric from "./hasGeneric";

export type UsingGenerics = {
    field1: basic.Maybe<string>;
    field2: basic.Either<string, number>;
};

export function isUsingGenerics(value: unknown): value is UsingGenerics {
    return svt.isInterface<UsingGenerics>(value, {field1: basic.isMaybe(svt.isString), field2: basic.isEither(svt.isString, svt.isNumber)});
}

export function validateUsingGenerics(value: unknown): svt.ValidationResult<UsingGenerics> {
    return svt.validate<UsingGenerics>(value, {field1: basic.validateMaybe(svt.validateString), field2: basic.validateEither(svt.validateString, svt.validateNumber)});
}

export type UsingOwnGenerics<T> = {
    field1: basic.Maybe<T>;
};

export function isUsingOwnGenerics<T>(isT: svt.TypePredicate<T>): svt.TypePredicate<UsingOwnGenerics<T>> {
    return function isUsingOwnGenericsT(value: unknown): value is UsingOwnGenerics<T> {
        return svt.isInterface<UsingOwnGenerics<T>>(value, {field1: basic.isMaybe(isT)});
    };
}

export function validateUsingOwnGenerics<T>(validateT: svt.Validator<T>): svt.Validator<UsingOwnGenerics<T>> {
    return function validateUsingOwnGenericsT(value: unknown): svt.ValidationResult<UsingOwnGenerics<T>> {
        return svt.validate<UsingOwnGenerics<T>>(value, {field1: basic.validateMaybe(validateT)});
    };
}

export type KnownForMovie = {
    media_type: "movie";
    poster_path: string | null | undefined;
    id: number;
    title: string | null | undefined;
    vote_average: number;
    release_date: string | null | undefined;
    overview: string;
};

export function isKnownForMovie(value: unknown): value is KnownForMovie {
    return svt.isInterface<KnownForMovie>(value, {media_type: "movie", poster_path: svt.optional(svt.isString), id: svt.isNumber, title: svt.optional(svt.isString), vote_average: svt.isNumber, release_date: svt.optional(svt.isString), overview: svt.isString});
}

export function validateKnownForMovie(value: unknown): svt.ValidationResult<KnownForMovie> {
    return svt.validate<KnownForMovie>(value, {media_type: "movie", poster_path: svt.validateOptional(svt.validateString), id: svt.validateNumber, title: svt.validateOptional(svt.validateString), vote_average: svt.validateNumber, release_date: svt.validateOptional(svt.validateString), overview: svt.validateString});
}

export type KnownForShow = {
    media_type: "tv";
    poster_path: string | null | undefined;
    id: number;
    vote_average: number;
    overview: string;
    first_air_date: string | null | undefined;
    name: string | null | undefined;
};

export function isKnownForShow(value: unknown): value is KnownForShow {
    return svt.isInterface<KnownForShow>(value, {media_type: "tv", poster_path: svt.optional(svt.isString), id: svt.isNumber, vote_average: svt.isNumber, overview: svt.isString, first_air_date: svt.optional(svt.isString), name: svt.optional(svt.isString)});
}

export function validateKnownForShow(value: unknown): svt.ValidationResult<KnownForShow> {
    return svt.validate<KnownForShow>(value, {media_type: "tv", poster_path: svt.validateOptional(svt.validateString), id: svt.validateNumber, vote_average: svt.validateNumber, overview: svt.validateString, first_air_date: svt.validateOptional(svt.validateString), name: svt.validateOptional(svt.validateString)});
}

export type KnownFor = KnownForShow | KnownForMovie | string | number;

export function isKnownFor(value: unknown): value is KnownFor {
    return [isKnownForShow, isKnownForMovie, svt.isString, svt.isNumber].some((typePredicate) => typePredicate(value));
}

export function validateKnownFor(value: unknown): svt.ValidationResult<KnownFor> {
    return svt.validateOneOf<KnownFor>(value, [validateKnownForShow, validateKnownForMovie, svt.validateString, svt.validateNumber]);
}

export type KnownForMovieWithoutTypeTag = {
    poster_path: string | null | undefined;
    id: number;
    title: string | null | undefined;
    vote_average: number;
    release_date: string | null | undefined;
    overview: string;
};

export function isKnownForMovieWithoutTypeTag(value: unknown): value is KnownForMovieWithoutTypeTag {
    return svt.isInterface<KnownForMovieWithoutTypeTag>(value, {poster_path: svt.optional(svt.isString), id: svt.isNumber, title: svt.optional(svt.isString), vote_average: svt.isNumber, release_date: svt.optional(svt.isString), overview: svt.isString});
}

export function validateKnownForMovieWithoutTypeTag(value: unknown): svt.ValidationResult<KnownForMovieWithoutTypeTag> {
    return svt.validate<KnownForMovieWithoutTypeTag>(value, {poster_path: svt.validateOptional(svt.validateString), id: svt.validateNumber, title: svt.validateOptional(svt.validateString), vote_average: svt.validateNumber, release_date: svt.validateOptional(svt.validateString), overview: svt.validateString});
}

export type KnownForShowWithoutTypeTag = {
    poster_path: string | null | undefined;
    id: number;
    vote_average: number;
    overview: string;
    first_air_date: string | null | undefined;
    name: string | null | undefined;
};

export function isKnownForShowWithoutTypeTag(value: unknown): value is KnownForShowWithoutTypeTag {
    return svt.isInterface<KnownForShowWithoutTypeTag>(value, {poster_path: svt.optional(svt.isString), id: svt.isNumber, vote_average: svt.isNumber, overview: svt.isString, first_air_date: svt.optional(svt.isString), name: svt.optional(svt.isString)});
}

export function validateKnownForShowWithoutTypeTag(value: unknown): svt.ValidationResult<KnownForShowWithoutTypeTag> {
    return svt.validate<KnownForShowWithoutTypeTag>(value, {poster_path: svt.validateOptional(svt.validateString), id: svt.validateNumber, vote_average: svt.validateNumber, overview: svt.validateString, first_air_date: svt.validateOptional(svt.validateString), name: svt.validateOptional(svt.validateString)});
}

export type KnownForEmbedded = MovieStartingWithLowercase | TvStartingWithLowercase;

export enum KnownForEmbeddedTag {
    MovieStartingWithLowercase = "movieStartingWithLowercase",
    TvStartingWithLowercase = "tvStartingWithLowercase",
}

export type MovieStartingWithLowercase = {
    media_type: KnownForEmbeddedTag.MovieStartingWithLowercase;
    poster_path: string | null | undefined;
    id: number;
    title: string | null | undefined;
    vote_average: number;
    release_date: string | null | undefined;
    overview: string;
};

export type TvStartingWithLowercase = {
    media_type: KnownForEmbeddedTag.TvStartingWithLowercase;
    poster_path: string | null | undefined;
    id: number;
    vote_average: number;
    overview: string;
    first_air_date: string | null | undefined;
    name: string | null | undefined;
};

export function MovieStartingWithLowercase(data: KnownForMovieWithoutTypeTag): MovieStartingWithLowercase {
    return {media_type: KnownForEmbeddedTag.MovieStartingWithLowercase, ...data};
}

export function TvStartingWithLowercase(data: KnownForShowWithoutTypeTag): TvStartingWithLowercase {
    return {media_type: KnownForEmbeddedTag.TvStartingWithLowercase, ...data};
}

export function isKnownForEmbedded(value: unknown): value is KnownForEmbedded {
    return [isMovieStartingWithLowercase, isTvStartingWithLowercase].some((typePredicate) => typePredicate(value));
}

export function isMovieStartingWithLowercase(value: unknown): value is MovieStartingWithLowercase {
    return svt.isInterface<MovieStartingWithLowercase>(value, {media_type: KnownForEmbeddedTag.MovieStartingWithLowercase, poster_path: svt.optional(svt.isString), id: svt.isNumber, title: svt.optional(svt.isString), vote_average: svt.isNumber, release_date: svt.optional(svt.isString), overview: svt.isString});
}

export function isTvStartingWithLowercase(value: unknown): value is TvStartingWithLowercase {
    return svt.isInterface<TvStartingWithLowercase>(value, {media_type: KnownForEmbeddedTag.TvStartingWithLowercase, poster_path: svt.optional(svt.isString), id: svt.isNumber, vote_average: svt.isNumber, overview: svt.isString, first_air_date: svt.optional(svt.isString), name: svt.optional(svt.isString)});
}

export function validateKnownForEmbedded(value: unknown): svt.ValidationResult<KnownForEmbedded> {
    return svt.validateWithTypeTag<KnownForEmbedded>(value, {[KnownForEmbeddedTag.MovieStartingWithLowercase]: validateMovieStartingWithLowercase, [KnownForEmbeddedTag.TvStartingWithLowercase]: validateTvStartingWithLowercase}, "media_type");
}

export function validateMovieStartingWithLowercase(value: unknown): svt.ValidationResult<MovieStartingWithLowercase> {
    return svt.validate<MovieStartingWithLowercase>(value, {media_type: KnownForEmbeddedTag.MovieStartingWithLowercase, poster_path: svt.validateOptional(svt.validateString), id: svt.validateNumber, title: svt.validateOptional(svt.validateString), vote_average: svt.validateNumber, release_date: svt.validateOptional(svt.validateString), overview: svt.validateString});
}

export function validateTvStartingWithLowercase(value: unknown): svt.ValidationResult<TvStartingWithLowercase> {
    return svt.validate<TvStartingWithLowercase>(value, {media_type: KnownForEmbeddedTag.TvStartingWithLowercase, poster_path: svt.validateOptional(svt.validateString), id: svt.validateNumber, vote_average: svt.validateNumber, overview: svt.validateString, first_air_date: svt.validateOptional(svt.validateString), name: svt.validateOptional(svt.validateString)});
}

export type KnownForEmbeddedWithUpperCase = Movie | Tv;

export enum KnownForEmbeddedWithUpperCaseTag {
    Movie = "Movie",
    Tv = "Tv",
}

export type Movie = {
    media_type: KnownForEmbeddedWithUpperCaseTag.Movie;
    poster_path: string | null | undefined;
    id: number;
    title: string | null | undefined;
    vote_average: number;
    release_date: string | null | undefined;
    overview: string;
};

export type Tv = {
    media_type: KnownForEmbeddedWithUpperCaseTag.Tv;
    poster_path: string | null | undefined;
    id: number;
    vote_average: number;
    overview: string;
    first_air_date: string | null | undefined;
    name: string | null | undefined;
};

export function Movie(data: KnownForMovieWithoutTypeTag): Movie {
    return {media_type: KnownForEmbeddedWithUpperCaseTag.Movie, ...data};
}

export function Tv(data: KnownForShowWithoutTypeTag): Tv {
    return {media_type: KnownForEmbeddedWithUpperCaseTag.Tv, ...data};
}

export function isKnownForEmbeddedWithUpperCase(value: unknown): value is KnownForEmbeddedWithUpperCase {
    return [isMovie, isTv].some((typePredicate) => typePredicate(value));
}

export function isMovie(value: unknown): value is Movie {
    return svt.isInterface<Movie>(value, {media_type: KnownForEmbeddedWithUpperCaseTag.Movie, poster_path: svt.optional(svt.isString), id: svt.isNumber, title: svt.optional(svt.isString), vote_average: svt.isNumber, release_date: svt.optional(svt.isString), overview: svt.isString});
}

export function isTv(value: unknown): value is Tv {
    return svt.isInterface<Tv>(value, {media_type: KnownForEmbeddedWithUpperCaseTag.Tv, poster_path: svt.optional(svt.isString), id: svt.isNumber, vote_average: svt.isNumber, overview: svt.isString, first_air_date: svt.optional(svt.isString), name: svt.optional(svt.isString)});
}

export function validateKnownForEmbeddedWithUpperCase(value: unknown): svt.ValidationResult<KnownForEmbeddedWithUpperCase> {
    return svt.validateWithTypeTag<KnownForEmbeddedWithUpperCase>(value, {[KnownForEmbeddedWithUpperCaseTag.Movie]: validateMovie, [KnownForEmbeddedWithUpperCaseTag.Tv]: validateTv}, "media_type");
}

export function validateMovie(value: unknown): svt.ValidationResult<Movie> {
    return svt.validate<Movie>(value, {media_type: KnownForEmbeddedWithUpperCaseTag.Movie, poster_path: svt.validateOptional(svt.validateString), id: svt.validateNumber, title: svt.validateOptional(svt.validateString), vote_average: svt.validateNumber, release_date: svt.validateOptional(svt.validateString), overview: svt.validateString});
}

export function validateTv(value: unknown): svt.ValidationResult<Tv> {
    return svt.validate<Tv>(value, {media_type: KnownForEmbeddedWithUpperCaseTag.Tv, poster_path: svt.validateOptional(svt.validateString), id: svt.validateNumber, vote_average: svt.validateNumber, overview: svt.validateString, first_air_date: svt.validateOptional(svt.validateString), name: svt.validateOptional(svt.validateString)});
}