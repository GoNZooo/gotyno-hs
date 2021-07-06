import * as svt from "simple-validation-tools";

export type Option<T> = None | Some<T>;

export enum OptionTag {
    None = "None",
    Some = "Some",
}

export type None = {
    type: OptionTag.None;
};

export type Some<T> = {
    type: OptionTag.Some;
    data: T;
};

export function None(): None {
    return {type: OptionTag.None};
}

export function Some<T>(data: T): Some<T> {
    return {type: OptionTag.Some, data};
}

export function isOption<T>(isT: svt.TypePredicate<T>): svt.TypePredicate<Option<T>> {
    return function isOptionT(value: unknown): value is Option<T> {
        return [isNone, isSome(isT)].some((typePredicate) => typePredicate(value));
    };
}

export function isNone(value: unknown): value is None {
    return svt.isInterface<None>(value, {type: OptionTag.None});
}

export function isSome<T>(isT: svt.TypePredicate<T>): svt.TypePredicate<Some<T>> {
    return function isSomeT(value: unknown): value is Some<T> {
        return svt.isInterface<Some<T>>(value, {type: OptionTag.Some, data: isT});
    };
}

export function validateOption<T>(validateT: svt.Validator<T>): svt.Validator<Option<T>> {
    return function validateOptionT(value: unknown): svt.ValidationResult<Option<T>> {
        return svt.validateWithTypeTag<Option<T>>(value, {[OptionTag.None]: validateNone, [OptionTag.Some]: validateSome(validateT)}, "type");
    };
}

export function validateNone(value: unknown): svt.ValidationResult<None> {
    return svt.validate<None>(value, {type: OptionTag.None});
}

export function validateSome<T>(validateT: svt.Validator<T>): svt.Validator<Some<T>> {
    return function validateSomeT(value: unknown): svt.ValidationResult<Some<T>> {
        return svt.validate<Some<T>>(value, {type: OptionTag.Some, data: validateT});
    };
}
