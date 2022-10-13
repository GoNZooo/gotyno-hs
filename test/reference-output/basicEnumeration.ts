import * as svt from "simple-validation-tools";

export enum StringValues {
    first = "first",
    second = "second",
    third = "Third",
    fourth = "Fourth",
}

export function isStringValues(value: unknown): value is StringValues {
    return [StringValues.first, StringValues.second, StringValues.third, StringValues.fourth].some((v) => v === value);
}

export function validateStringValues(value: unknown): svt.ValidationResult<StringValues> {
    return svt.validateOneOfLiterals<StringValues>(value, [StringValues.first, StringValues.second, StringValues.third, StringValues.fourth]);
}

export enum IntValues {
    first = 1,
    second = 2,
    third = 3,
    fourth = 4,
}

export function isIntValues(value: unknown): value is IntValues {
    return [IntValues.first, IntValues.second, IntValues.third, IntValues.fourth].some((v) => v === value);
}

export function validateIntValues(value: unknown): svt.ValidationResult<IntValues> {
    return svt.validateOneOfLiterals<IntValues>(value, [IntValues.first, IntValues.second, IntValues.third, IntValues.fourth]);
}
