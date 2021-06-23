import * as svt from "simple-validation-tools";

import * as hasGeneric from "./hasGeneric";

export type Holder<T> = {
    value: T;
};

export function isHolder<T>(isT: svt.TypePredicate<T>): svt.TypePredicate<Holder<T>> {
    return function isHolderT(value: unknown): value is Holder<T> {
        return svt.isInterface<Holder<T>>(value, {value: isT});
    };
}

export function validateHolder<T>(validateT: svt.Validator<T>): svt.Validator<Holder<T>> {
    return function validateHolderT(value: unknown): svt.ValidationResult<Holder<T>> {
        return svt.validate<Holder<T>>(value, {value: validateT});
    };
}

export type MaybeHolder<T> = {
    value: T | null | undefined;
};

export function isMaybeHolder<T>(isT: svt.TypePredicate<T>): svt.TypePredicate<MaybeHolder<T>> {
    return function isMaybeHolderT(value: unknown): value is MaybeHolder<T> {
        return svt.isInterface<MaybeHolder<T>>(value, {value: svt.optional(isT)});
    };
}

export function validateMaybeHolder<T>(validateT: svt.Validator<T>): svt.Validator<MaybeHolder<T>> {
    return function validateMaybeHolderT(value: unknown): svt.ValidationResult<MaybeHolder<T>> {
        return svt.validate<MaybeHolder<T>>(value, {value: svt.validateOptional(validateT)});
    };
}

export type Either<L, R> = Left<L> | Right<R>;

export enum EitherTag {
    Left = "Left",
    Right = "Right",
}

export type Left<L> = {
    type: EitherTag.Left;
    data: L;
};

export type Right<R> = {
    type: EitherTag.Right;
    data: R;
};

export function Left<L>(data: L): Left<L> {
    return {type: EitherTag.Left, data};
}

export function Right<R>(data: R): Right<R> {
    return {type: EitherTag.Right, data};
}

export function isEither<L, R>(isL: svt.TypePredicate<L>, isR: svt.TypePredicate<R>): svt.TypePredicate<Either<L, R>> {
    return function isEitherLR(value: unknown): value is Either<L, R> {
        return [isLeft(isL), isRight(isR)].some((typePredicate) => typePredicate(value));
    };
}

export function isLeft<L>(isL: svt.TypePredicate<L>): svt.TypePredicate<Left<L>> {
    return function isLeftL(value: unknown): value is Left<L> {
        return svt.isInterface<Left<L>>(value, {type: EitherTag.Left, data: isL});
    };
}

export function isRight<R>(isR: svt.TypePredicate<R>): svt.TypePredicate<Right<R>> {
    return function isRightR(value: unknown): value is Right<R> {
        return svt.isInterface<Right<R>>(value, {type: EitherTag.Right, data: isR});
    };
}

export function validateEither<L, R>(validateL: svt.Validator<L>, validateR: svt.Validator<R>): svt.Validator<Either<L, R>> {
    return function validateEitherLR(value: unknown): svt.ValidationResult<Either<L, R>> {
        return svt.validateWithTypeTag<Either<L, R>>(value, {[EitherTag.Left]: validateLeft(validateL), [EitherTag.Right]: validateRight(validateR)}, "type");
    };
}

export function validateLeft<L>(validateL: svt.Validator<L>): svt.Validator<Left<L>> {
    return function validateLeftL(value: unknown): svt.ValidationResult<Left<L>> {
        return svt.validate<Left<L>>(value, {type: EitherTag.Left, data: validateL});
    };
}

export function validateRight<R>(validateR: svt.Validator<R>): svt.Validator<Right<R>> {
    return function validateRightR(value: unknown): svt.ValidationResult<Right<R>> {
        return svt.validate<Right<R>>(value, {type: EitherTag.Right, data: validateR});
    };
}

export type UsingGenerics = {
    field1: hasGeneric.Maybe<string>;
    field2: Either<string, number>;
};

export function isUsingGenerics(value: unknown): value is UsingGenerics {
    return svt.isInterface<UsingGenerics>(value, {field1: hasGeneric.isMaybe(svt.isString), field2: isEither(svt.isString, svt.isNumber)});
}

export function validateUsingGenerics(value: unknown): svt.ValidationResult<UsingGenerics> {
    return svt.validate<UsingGenerics>(value, {field1: hasGeneric.validateMaybe(svt.validateString), field2: validateEither(svt.validateString, svt.validateNumber)});
}

export type UsingOwnGenerics<T> = {
    field1: hasGeneric.Maybe<T>;
};

export function isUsingOwnGenerics<T>(isT: svt.TypePredicate<T>): svt.TypePredicate<UsingOwnGenerics<T>> {
    return function isUsingOwnGenericsT(value: unknown): value is UsingOwnGenerics<T> {
        return svt.isInterface<UsingOwnGenerics<T>>(value, {field1: hasGeneric.isMaybe(isT)});
    };
}

export function validateUsingOwnGenerics<T>(validateT: svt.Validator<T>): svt.Validator<UsingOwnGenerics<T>> {
    return function validateUsingOwnGenericsT(value: unknown): svt.ValidationResult<UsingOwnGenerics<T>> {
        return svt.validate<UsingOwnGenerics<T>>(value, {field1: hasGeneric.validateMaybe(validateT)});
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

export type KnownForEmbedded = Movie | TV;

export enum KnownForEmbeddedTag {
    Movie = "Movie",
    TV = "TV",
}

export type Movie = {
    media_type: KnownForEmbeddedTag.Movie;
    poster_path: string | null | undefined;
    id: number;
    title: string | null | undefined;
    vote_average: number;
    release_date: string | null | undefined;
    overview: string;
};

export type TV = {
    media_type: KnownForEmbeddedTag.TV;
    poster_path: string | null | undefined;
    id: number;
    vote_average: number;
    overview: string;
    first_air_date: string | null | undefined;
    name: string | null | undefined;
};

export function Movie(data: KnownForMovieWithoutTypeTag): Movie {
    return {media_type: KnownForEmbeddedTag.Movie, ...data};
}

export function TV(data: KnownForShowWithoutTypeTag): TV {
    return {media_type: KnownForEmbeddedTag.TV, ...data};
}

export function isKnownForEmbedded(value: unknown): value is KnownForEmbedded {
    return [isMovie, isTV].some((typePredicate) => typePredicate(value));
}

export function isMovie(value: unknown): value is Movie {
    return svt.isInterface<Movie>(value, {media_type: KnownForEmbeddedTag.Movie, poster_path: svt.optional(svt.isString), id: svt.isNumber, title: svt.optional(svt.isString), vote_average: svt.isNumber, release_date: svt.optional(svt.isString), overview: svt.isString});
}

export function isTV(value: unknown): value is TV {
    return svt.isInterface<TV>(value, {media_type: KnownForEmbeddedTag.TV, poster_path: svt.optional(svt.isString), id: svt.isNumber, vote_average: svt.isNumber, overview: svt.isString, first_air_date: svt.optional(svt.isString), name: svt.optional(svt.isString)});
}

export function validateKnownForEmbedded(value: unknown): svt.ValidationResult<KnownForEmbedded> {
    return svt.validateWithTypeTag<KnownForEmbedded>(value, {[KnownForEmbeddedTag.Movie]: validateMovie, [KnownForEmbeddedTag.TV]: validateTV}, "media_type");
}

export function validateMovie(value: unknown): svt.ValidationResult<Movie> {
    return svt.validate<Movie>(value, {media_type: KnownForEmbeddedTag.Movie, poster_path: svt.validateOptional(svt.validateString), id: svt.validateNumber, title: svt.validateOptional(svt.validateString), vote_average: svt.validateNumber, release_date: svt.validateOptional(svt.validateString), overview: svt.validateString});
}

export function validateTV(value: unknown): svt.ValidationResult<TV> {
    return svt.validate<TV>(value, {media_type: KnownForEmbeddedTag.TV, poster_path: svt.validateOptional(svt.validateString), id: svt.validateNumber, vote_average: svt.validateNumber, overview: svt.validateString, first_air_date: svt.validateOptional(svt.validateString), name: svt.validateOptional(svt.validateString)});
}