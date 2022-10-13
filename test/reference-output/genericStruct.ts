import * as svt from "simple-validation-tools";

export type GenericStruct<T> = {
    field: T;
};

export function isGenericStruct<T>(isT: svt.TypePredicate<T>): svt.TypePredicate<GenericStruct<T>> {
    return function isGenericStructT(value: unknown): value is GenericStruct<T> {
        return svt.isInterface<GenericStruct<T>>(value, {field: isT});
    };
}

export function validateGenericStruct<T>(validateT: svt.Validator<T>): svt.Validator<GenericStruct<T>> {
    return function validateGenericStructT(value: unknown): svt.ValidationResult<GenericStruct<T>> {
        return svt.validate<GenericStruct<T>>(value, {field: validateT});
    };
}
