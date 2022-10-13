import * as svt from "simple-validation-tools";

export type BasicStruct = {
    field1: number;
    field2: string;
};

export function isBasicStruct(value: unknown): value is BasicStruct {
    return svt.isInterface<BasicStruct>(value, {field1: svt.isNumber, field2: svt.isString});
}

export function validateBasicStruct(value: unknown): svt.ValidationResult<BasicStruct> {
    return svt.validate<BasicStruct>(value, {field1: svt.validateNumber, field2: svt.validateString});
}
