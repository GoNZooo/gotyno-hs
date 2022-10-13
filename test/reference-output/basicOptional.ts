import * as svt from "simple-validation-tools";

export type HasOptionalString = {
    stringField?: string;
    optionalArrayField?: number[];
    arrayOfOptionalField: (number | null | undefined)[];
};

export function isHasOptionalString(value: unknown): value is HasOptionalString {
    return svt.isInterface<HasOptionalString>(value, {stringField: svt.optional(svt.isString), optionalArrayField: svt.optional(svt.arrayOf(svt.isNumber)), arrayOfOptionalField: svt.arrayOf(svt.optional(svt.isNumber))});
}

export function validateHasOptionalString(value: unknown): svt.ValidationResult<HasOptionalString> {
    return svt.validate<HasOptionalString>(value, {stringField: svt.validateOptional(svt.validateString), optionalArrayField: svt.validateOptional(svt.validateArray(svt.validateNumber)), arrayOfOptionalField: svt.validateArray(svt.validateOptional(svt.validateNumber))});
}

export type HasOptionalConstructor = DoesNot | Does | HasOptionalStruct;

export enum HasOptionalConstructorTag {
    DoesNot = "DoesNot",
    Does = "Does",
    HasOptionalStruct = "HasOptionalStruct",
}

export type DoesNot = {
    type: HasOptionalConstructorTag.DoesNot;
    data: number;
};

export type Does = {
    type: HasOptionalConstructorTag.Does;
    data?: number;
};

export type HasOptionalStruct = {
    type: HasOptionalConstructorTag.HasOptionalStruct;
    data?: HasOptionalString;
};

export function DoesNot(data: number): DoesNot {
    return {type: HasOptionalConstructorTag.DoesNot, data};
}

export function Does(data?: number): Does {
    return {type: HasOptionalConstructorTag.Does, data};
}

export function HasOptionalStruct(data?: HasOptionalString): HasOptionalStruct {
    return {type: HasOptionalConstructorTag.HasOptionalStruct, data};
}

export function isHasOptionalConstructor(value: unknown): value is HasOptionalConstructor {
    return [isDoesNot, isDoes, isHasOptionalStruct].some((typePredicate) => typePredicate(value));
}

export function isDoesNot(value: unknown): value is DoesNot {
    return svt.isInterface<DoesNot>(value, {type: HasOptionalConstructorTag.DoesNot, data: svt.isNumber});
}

export function isDoes(value: unknown): value is Does {
    return svt.isInterface<Does>(value, {type: HasOptionalConstructorTag.Does, data: svt.optional(svt.isNumber)});
}

export function isHasOptionalStruct(value: unknown): value is HasOptionalStruct {
    return svt.isInterface<HasOptionalStruct>(value, {type: HasOptionalConstructorTag.HasOptionalStruct, data: svt.optional(isHasOptionalString)});
}

export function validateHasOptionalConstructor(value: unknown): svt.ValidationResult<HasOptionalConstructor> {
    return svt.validateWithTypeTag<HasOptionalConstructor>(value, {[HasOptionalConstructorTag.DoesNot]: validateDoesNot, [HasOptionalConstructorTag.Does]: validateDoes, [HasOptionalConstructorTag.HasOptionalStruct]: validateHasOptionalStruct}, "type");
}

export function validateDoesNot(value: unknown): svt.ValidationResult<DoesNot> {
    return svt.validate<DoesNot>(value, {type: HasOptionalConstructorTag.DoesNot, data: svt.validateNumber});
}

export function validateDoes(value: unknown): svt.ValidationResult<Does> {
    return svt.validate<Does>(value, {type: HasOptionalConstructorTag.Does, data: svt.validateOptional(svt.validateNumber)});
}

export function validateHasOptionalStruct(value: unknown): svt.ValidationResult<HasOptionalStruct> {
    return svt.validate<HasOptionalStruct>(value, {type: HasOptionalConstructorTag.HasOptionalStruct, data: svt.validateOptional(validateHasOptionalString)});
}
