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