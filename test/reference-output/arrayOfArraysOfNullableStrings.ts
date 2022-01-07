import * as svt from "simple-validation-tools";

export type Type = {
    field: (string | null | undefined)[][];
};

export function isType(value: unknown): value is Type {
    return svt.isInterface<Type>(value, {field: svt.arrayOf(svt.arrayOf(svt.optional(svt.isString)))});
}

export function validateType(value: unknown): svt.ValidationResult<Type> {
    return svt.validate<Type>(value, {field: svt.validateArray(svt.validateArray(svt.validateOptional(svt.validateString)))});
}