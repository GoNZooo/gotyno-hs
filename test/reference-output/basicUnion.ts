import * as svt from "simple-validation-tools";

export type PayloadStruct = {
    field1: number;
};

export function isPayloadStruct(value: unknown): value is PayloadStruct {
    return svt.isInterface<PayloadStruct>(value, {field1: svt.isNumber});
}

export function validatePayloadStruct(value: unknown): svt.ValidationResult<PayloadStruct> {
    return svt.validate<PayloadStruct>(value, {field1: svt.validateNumber});
}

export type BasicUnion = HasStringPayload | HasPayload | HasNoPayload;

export enum BasicUnionTag {
    HasStringPayload = "HasStringPayload",
    HasPayload = "HasPayload",
    HasNoPayload = "HasNoPayload",
}

export type HasStringPayload = {
    type: BasicUnionTag.HasStringPayload;
    data: string;
};

export type HasPayload = {
    type: BasicUnionTag.HasPayload;
    data: PayloadStruct;
};

export type HasNoPayload = {
    type: BasicUnionTag.HasNoPayload;
};

export function HasStringPayload(data: string): HasStringPayload {
    return {type: BasicUnionTag.HasStringPayload, data};
}

export function HasPayload(data: PayloadStruct): HasPayload {
    return {type: BasicUnionTag.HasPayload, data};
}

export function HasNoPayload(): HasNoPayload {
    return {type: BasicUnionTag.HasNoPayload};
}

export function isBasicUnion(value: unknown): value is BasicUnion {
    return [isHasStringPayload, isHasPayload, isHasNoPayload].some((typePredicate) => typePredicate(value));
}

export function isHasStringPayload(value: unknown): value is HasStringPayload {
    return svt.isInterface<HasStringPayload>(value, {type: BasicUnionTag.HasStringPayload, data: svt.isString});
}

export function isHasPayload(value: unknown): value is HasPayload {
    return svt.isInterface<HasPayload>(value, {type: BasicUnionTag.HasPayload, data: isPayloadStruct});
}

export function isHasNoPayload(value: unknown): value is HasNoPayload {
    return svt.isInterface<HasNoPayload>(value, {type: BasicUnionTag.HasNoPayload});
}

export function validateBasicUnion(value: unknown): svt.ValidationResult<BasicUnion> {
    return svt.validateWithTypeTag<BasicUnion>(value, {[BasicUnionTag.HasStringPayload]: validateHasStringPayload, [BasicUnionTag.HasPayload]: validateHasPayload, [BasicUnionTag.HasNoPayload]: validateHasNoPayload}, "type");
}

export function validateHasStringPayload(value: unknown): svt.ValidationResult<HasStringPayload> {
    return svt.validate<HasStringPayload>(value, {type: BasicUnionTag.HasStringPayload, data: svt.validateString});
}

export function validateHasPayload(value: unknown): svt.ValidationResult<HasPayload> {
    return svt.validate<HasPayload>(value, {type: BasicUnionTag.HasPayload, data: validatePayloadStruct});
}

export function validateHasNoPayload(value: unknown): svt.ValidationResult<HasNoPayload> {
    return svt.validate<HasNoPayload>(value, {type: BasicUnionTag.HasNoPayload});
}
