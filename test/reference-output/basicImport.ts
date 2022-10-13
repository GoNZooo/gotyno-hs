import * as svt from "simple-validation-tools";

import * as basicStruct from "./basicStruct";

export type StructUsingImport = {
    field: basicStruct.BasicStruct;
};

export function isStructUsingImport(value: unknown): value is StructUsingImport {
    return svt.isInterface<StructUsingImport>(value, {field: basicStruct.isBasicStruct});
}

export function validateStructUsingImport(value: unknown): svt.ValidationResult<StructUsingImport> {
    return svt.validate<StructUsingImport>(value, {field: basicStruct.validateBasicStruct});
}

export type UnionUsingImport = ConstructorWithPayload;

export enum UnionUsingImportTag {
    ConstructorWithPayload = "ConstructorWithPayload",
}

export type ConstructorWithPayload = {
    type: UnionUsingImportTag.ConstructorWithPayload;
    data: basicStruct.BasicStruct;
};

export function ConstructorWithPayload(data: basicStruct.BasicStruct): ConstructorWithPayload {
    return {type: UnionUsingImportTag.ConstructorWithPayload, data};
}

export function isUnionUsingImport(value: unknown): value is UnionUsingImport {
    return [isConstructorWithPayload].some((typePredicate) => typePredicate(value));
}

export function isConstructorWithPayload(value: unknown): value is ConstructorWithPayload {
    return svt.isInterface<ConstructorWithPayload>(value, {type: UnionUsingImportTag.ConstructorWithPayload, data: basicStruct.isBasicStruct});
}

export function validateUnionUsingImport(value: unknown): svt.ValidationResult<UnionUsingImport> {
    return svt.validateWithTypeTag<UnionUsingImport>(value, {[UnionUsingImportTag.ConstructorWithPayload]: validateConstructorWithPayload}, "type");
}

export function validateConstructorWithPayload(value: unknown): svt.ValidationResult<ConstructorWithPayload> {
    return svt.validate<ConstructorWithPayload>(value, {type: UnionUsingImportTag.ConstructorWithPayload, data: basicStruct.validateBasicStruct});
}
