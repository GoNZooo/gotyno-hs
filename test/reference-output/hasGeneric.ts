import * as svt from "simple-validation-tools";

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