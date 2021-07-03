import * as svt from "simple-validation-tools";

import * as basic from "./basic";

export type UsesImport = {
    type: "UsesImport";
    recruiter: basic.Recruiter;
};

export function isUsesImport(value: unknown): value is UsesImport {
    return svt.isInterface<UsesImport>(value, {type: "UsesImport", recruiter: basic.isRecruiter});
}

export function validateUsesImport(value: unknown): svt.ValidationResult<UsesImport> {
    return svt.validate<UsesImport>(value, {type: "UsesImport", recruiter: basic.validateRecruiter});
}

export type HoldsSomething<T> = {
    holdingField: T;
};

export function isHoldsSomething<T>(isT: svt.TypePredicate<T>): svt.TypePredicate<HoldsSomething<T>> {
    return function isHoldsSomethingT(value: unknown): value is HoldsSomething<T> {
        return svt.isInterface<HoldsSomething<T>>(value, {holdingField: isT});
    };
}

export function validateHoldsSomething<T>(validateT: svt.Validator<T>): svt.Validator<HoldsSomething<T>> {
    return function validateHoldsSomethingT(value: unknown): svt.ValidationResult<HoldsSomething<T>> {
        return svt.validate<HoldsSomething<T>>(value, {holdingField: validateT});
    };
}

export type StructureUsingImport = {
    event: basic.Event;
};

export function isStructureUsingImport(value: unknown): value is StructureUsingImport {
    return svt.isInterface<StructureUsingImport>(value, {event: basic.isEvent});
}

export function validateStructureUsingImport(value: unknown): svt.ValidationResult<StructureUsingImport> {
    return svt.validate<StructureUsingImport>(value, {event: basic.validateEvent});
}

export type UnionUsingImport = CoolEvent | Other;

export enum UnionUsingImportTag {
    CoolEvent = "CoolEvent",
    Other = "Other",
}

export type CoolEvent = {
    type: UnionUsingImportTag.CoolEvent;
    data: basic.Event;
};

export type Other = {
    type: UnionUsingImportTag.Other;
    data: basic.Person;
};

export function CoolEvent(data: basic.Event): CoolEvent {
    return {type: UnionUsingImportTag.CoolEvent, data};
}

export function Other(data: basic.Person): Other {
    return {type: UnionUsingImportTag.Other, data};
}

export function isUnionUsingImport(value: unknown): value is UnionUsingImport {
    return [isCoolEvent, isOther].some((typePredicate) => typePredicate(value));
}

export function isCoolEvent(value: unknown): value is CoolEvent {
    return svt.isInterface<CoolEvent>(value, {type: UnionUsingImportTag.CoolEvent, data: basic.isEvent});
}

export function isOther(value: unknown): value is Other {
    return svt.isInterface<Other>(value, {type: UnionUsingImportTag.Other, data: basic.isPerson});
}

export function validateUnionUsingImport(value: unknown): svt.ValidationResult<UnionUsingImport> {
    return svt.validateWithTypeTag<UnionUsingImport>(value, {[UnionUsingImportTag.CoolEvent]: validateCoolEvent, [UnionUsingImportTag.Other]: validateOther}, "type");
}

export function validateCoolEvent(value: unknown): svt.ValidationResult<CoolEvent> {
    return svt.validate<CoolEvent>(value, {type: UnionUsingImportTag.CoolEvent, data: basic.validateEvent});
}

export function validateOther(value: unknown): svt.ValidationResult<Other> {
    return svt.validate<Other>(value, {type: UnionUsingImportTag.Other, data: basic.validatePerson});
}

export type AllConcrete = {
    field: HoldsSomething<basic.Either<basic.Maybe<StructureUsingImport>, UnionUsingImport>>;
};

export function isAllConcrete(value: unknown): value is AllConcrete {
    return svt.isInterface<AllConcrete>(value, {field: isHoldsSomething(basic.isEither(basic.isMaybe(isStructureUsingImport), isUnionUsingImport))});
}

export function validateAllConcrete(value: unknown): svt.ValidationResult<AllConcrete> {
    return svt.validate<AllConcrete>(value, {field: validateHoldsSomething(basic.validateEither(basic.validateMaybe(validateStructureUsingImport), validateUnionUsingImport))});
}