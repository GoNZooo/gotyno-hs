import * as svt from "simple-validation-tools";

export type Maybe<T> = Nothing | Just<T>;

export enum MaybeTag {
    Nothing = "Nothing",
    Just = "Just",
}

export type Nothing = {
    type: MaybeTag.Nothing;
};

export type Just<T> = {
    type: MaybeTag.Just;
    data: T;
};

export function Nothing(): Nothing {
    return {type: MaybeTag.Nothing};
}

export function Just<T>(data: T): Just<T> {
    return {type: MaybeTag.Just, data};
}

export function isMaybe<T>(isT: svt.TypePredicate<T>): svt.TypePredicate<Maybe<T>> {
    return function isMaybeT(value: unknown): value is Maybe<T> {
        return [isNothing, isJust(isT)].some((typePredicate) => typePredicate(value));
    };
}

export function isNothing(value: unknown): value is Nothing {
    return svt.isInterface<Nothing>(value, {type: MaybeTag.Nothing});
}

export function isJust<T>(isT: svt.TypePredicate<T>): svt.TypePredicate<Just<T>> {
    return function isJustT(value: unknown): value is Just<T> {
        return svt.isInterface<Just<T>>(value, {type: MaybeTag.Just, data: isT});
    };
}

export function validateMaybe<T>(validateT: svt.Validator<T>): svt.Validator<Maybe<T>> {
    return function validateMaybeT(value: unknown): svt.ValidationResult<Maybe<T>> {
        return svt.validateWithTypeTag<Maybe<T>>(value, {[MaybeTag.Nothing]: validateNothing, [MaybeTag.Just]: validateJust(validateT)}, "type");
    };
}

export function validateNothing(value: unknown): svt.ValidationResult<Nothing> {
    return svt.validate<Nothing>(value, {type: MaybeTag.Nothing});
}

export function validateJust<T>(validateT: svt.Validator<T>): svt.Validator<Just<T>> {
    return function validateJustT(value: unknown): svt.ValidationResult<Just<T>> {
        return svt.validate<Just<T>>(value, {type: MaybeTag.Just, data: validateT});
    };
}

export type Result<T, E> = Success<T> | Failure<E>;

export enum ResultTag {
    Success = "Success",
    Failure = "Failure",
}

export type Success<T> = {
    type: ResultTag.Success;
    data: T;
};

export type Failure<E> = {
    type: ResultTag.Failure;
    data: E;
};

export function Success<T>(data: T): Success<T> {
    return {type: ResultTag.Success, data};
}

export function Failure<E>(data: E): Failure<E> {
    return {type: ResultTag.Failure, data};
}

export function isResult<T, E>(isT: svt.TypePredicate<T>, isE: svt.TypePredicate<E>): svt.TypePredicate<Result<T, E>> {
    return function isResultTE(value: unknown): value is Result<T, E> {
        return [isSuccess(isT), isFailure(isE)].some((typePredicate) => typePredicate(value));
    };
}

export function isSuccess<T>(isT: svt.TypePredicate<T>): svt.TypePredicate<Success<T>> {
    return function isSuccessT(value: unknown): value is Success<T> {
        return svt.isInterface<Success<T>>(value, {type: ResultTag.Success, data: isT});
    };
}

export function isFailure<E>(isE: svt.TypePredicate<E>): svt.TypePredicate<Failure<E>> {
    return function isFailureE(value: unknown): value is Failure<E> {
        return svt.isInterface<Failure<E>>(value, {type: ResultTag.Failure, data: isE});
    };
}

export function validateResult<T, E>(validateT: svt.Validator<T>, validateE: svt.Validator<E>): svt.Validator<Result<T, E>> {
    return function validateResultTE(value: unknown): svt.ValidationResult<Result<T, E>> {
        return svt.validateWithTypeTag<Result<T, E>>(value, {[ResultTag.Success]: validateSuccess(validateT), [ResultTag.Failure]: validateFailure(validateE)}, "type");
    };
}

export function validateSuccess<T>(validateT: svt.Validator<T>): svt.Validator<Success<T>> {
    return function validateSuccessT(value: unknown): svt.ValidationResult<Success<T>> {
        return svt.validate<Success<T>>(value, {type: ResultTag.Success, data: validateT});
    };
}

export function validateFailure<E>(validateE: svt.Validator<E>): svt.Validator<Failure<E>> {
    return function validateFailureE(value: unknown): svt.ValidationResult<Failure<E>> {
        return svt.validate<Failure<E>>(value, {type: ResultTag.Failure, data: validateE});
    };
}