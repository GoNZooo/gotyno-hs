import * as svt from "simple-validation-tools";

export type GenericUnion<T> = HasTPayload<T> | HasNoPayload;

export enum GenericUnionTag {
    HasTPayload = "HasTPayload",
    HasNoPayload = "HasNoPayload",
}

export type HasTPayload<T> = {
    type: GenericUnionTag.HasTPayload;
    data: T;
};

export type HasNoPayload = {
    type: GenericUnionTag.HasNoPayload;
};

export function HasTPayload<T>(data: T): HasTPayload<T> {
    return {type: GenericUnionTag.HasTPayload, data};
}

export function HasNoPayload(): HasNoPayload {
    return {type: GenericUnionTag.HasNoPayload};
}

export function isGenericUnion<T>(isT: svt.TypePredicate<T>): svt.TypePredicate<GenericUnion<T>> {
    return function isGenericUnionT(value: unknown): value is GenericUnion<T> {
        return [isHasTPayload(isT), isHasNoPayload].some((typePredicate) => typePredicate(value));
    };
}

export function isHasTPayload<T>(isT: svt.TypePredicate<T>): svt.TypePredicate<HasTPayload<T>> {
    return function isHasTPayloadT(value: unknown): value is HasTPayload<T> {
        return svt.isInterface<HasTPayload<T>>(value, {type: GenericUnionTag.HasTPayload, data: isT});
    };
}

export function isHasNoPayload(value: unknown): value is HasNoPayload {
    return svt.isInterface<HasNoPayload>(value, {type: GenericUnionTag.HasNoPayload});
}

export function validateGenericUnion<T>(validateT: svt.Validator<T>): svt.Validator<GenericUnion<T>> {
    return function validateGenericUnionT(value: unknown): svt.ValidationResult<GenericUnion<T>> {
        return svt.validateWithTypeTag<GenericUnion<T>>(value, {[GenericUnionTag.HasTPayload]: validateHasTPayload(validateT), [GenericUnionTag.HasNoPayload]: validateHasNoPayload}, "type");
    };
}

export function validateHasTPayload<T>(validateT: svt.Validator<T>): svt.Validator<HasTPayload<T>> {
    return function validateHasTPayloadT(value: unknown): svt.ValidationResult<HasTPayload<T>> {
        return svt.validate<HasTPayload<T>>(value, {type: GenericUnionTag.HasTPayload, data: validateT});
    };
}

export function validateHasNoPayload(value: unknown): svt.ValidationResult<HasNoPayload> {
    return svt.validate<HasNoPayload>(value, {type: GenericUnionTag.HasNoPayload});
}
