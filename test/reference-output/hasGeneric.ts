import * as svt from "simple-validation-tools";

import * as external from "./external";
import * as other from "./other";

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
    value: external.Option<T>;
    otherValue: other.Plain;
};

export function isMaybeHolder<T>(isT: svt.TypePredicate<T>): svt.TypePredicate<MaybeHolder<T>> {
    return function isMaybeHolderT(value: unknown): value is MaybeHolder<T> {
        return svt.isInterface<MaybeHolder<T>>(value, {value: external.isOption(isT), otherValue: other.isPlain});
    };
}

export function validateMaybeHolder<T>(validateT: svt.Validator<T>): svt.Validator<MaybeHolder<T>> {
    return function validateMaybeHolderT(value: unknown): svt.ValidationResult<MaybeHolder<T>> {
        return svt.validate<MaybeHolder<T>>(value, {value: external.validateOption(validateT), otherValue: other.validatePlain});
    };
}

export type HasGenericEvent<T> = PlainEvent | GenericEvent<T>;

export enum HasGenericEventTag {
    PlainEvent = "PlainEvent",
    GenericEvent = "GenericEvent",
}

export type PlainEvent = {
    type: HasGenericEventTag.PlainEvent;
    data: other.Plain;
};

export type GenericEvent<T> = {
    type: HasGenericEventTag.GenericEvent;
    data: external.Option<T>;
};

export function PlainEvent(data: other.Plain): PlainEvent {
    return {type: HasGenericEventTag.PlainEvent, data};
}

export function GenericEvent<T>(data: external.Option<T>): GenericEvent<T> {
    return {type: HasGenericEventTag.GenericEvent, data};
}

export function isHasGenericEvent<T>(isT: svt.TypePredicate<T>): svt.TypePredicate<HasGenericEvent<T>> {
    return function isHasGenericEventT(value: unknown): value is HasGenericEvent<T> {
        return [isPlainEvent, isGenericEvent(isT)].some((typePredicate) => typePredicate(value));
    };
}

export function isPlainEvent(value: unknown): value is PlainEvent {
    return svt.isInterface<PlainEvent>(value, {type: HasGenericEventTag.PlainEvent, data: other.isPlain});
}

export function isGenericEvent<T>(isT: svt.TypePredicate<T>): svt.TypePredicate<GenericEvent<T>> {
    return function isGenericEventT(value: unknown): value is GenericEvent<T> {
        return svt.isInterface<GenericEvent<T>>(value, {type: HasGenericEventTag.GenericEvent, data: external.isOption(isT)});
    };
}

export function validateHasGenericEvent<T>(validateT: svt.Validator<T>): svt.Validator<HasGenericEvent<T>> {
    return function validateHasGenericEventT(value: unknown): svt.ValidationResult<HasGenericEvent<T>> {
        return svt.validateWithTypeTag<HasGenericEvent<T>>(value, {[HasGenericEventTag.PlainEvent]: validatePlainEvent, [HasGenericEventTag.GenericEvent]: validateGenericEvent(validateT)}, "type");
    };
}

export function validatePlainEvent(value: unknown): svt.ValidationResult<PlainEvent> {
    return svt.validate<PlainEvent>(value, {type: HasGenericEventTag.PlainEvent, data: other.validatePlain});
}

export function validateGenericEvent<T>(validateT: svt.Validator<T>): svt.Validator<GenericEvent<T>> {
    return function validateGenericEventT(value: unknown): svt.ValidationResult<GenericEvent<T>> {
        return svt.validate<GenericEvent<T>>(value, {type: HasGenericEventTag.GenericEvent, data: external.validateOption(validateT)});
    };
}
