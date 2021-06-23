import * as svt from "simple-validation-tools";

export type Recruiter = {
    type: "Recruiter";
    name: string;
    emails: (string | null | undefined)[];
    recruiter: Recruiter | null | undefined;
};

export function isRecruiter(value: unknown): value is Recruiter {
    return svt.isInterface<Recruiter>(value, {type: "Recruiter", name: svt.isString, emails: svt.arrayOf(svt.optional(svt.isString)), recruiter: svt.optional(isRecruiter)});
}

export function validateRecruiter(value: unknown): svt.ValidationResult<Recruiter> {
    return svt.validate<Recruiter>(value, {type: "Recruiter", name: svt.validateString, emails: svt.validateArray(svt.validateOptional(svt.validateString)), recruiter: svt.validateOptional(validateRecruiter)});
}

export type GetSearchesFilter = SearchesByQueryLike | SearchesByResultLike | NoSearchesFilter;

export enum GetSearchesFilterTag {
    SearchesByQueryLike = "SearchesByQueryLike",
    SearchesByResultLike = "SearchesByResultLike",
    NoSearchesFilter = "NoSearchesFilter",
}

export type SearchesByQueryLike = {
    type: GetSearchesFilterTag.SearchesByQueryLike;
    data: string;
};

export type SearchesByResultLike = {
    type: GetSearchesFilterTag.SearchesByResultLike;
    data: string;
};

export type NoSearchesFilter = {
    type: GetSearchesFilterTag.NoSearchesFilter;
};

export function SearchesByQueryLike(data: string): SearchesByQueryLike {
    return {type: GetSearchesFilterTag.SearchesByQueryLike, data};
}

export function SearchesByResultLike(data: string): SearchesByResultLike {
    return {type: GetSearchesFilterTag.SearchesByResultLike, data};
}

export function NoSearchesFilter(): NoSearchesFilter {
    return {type: GetSearchesFilterTag.NoSearchesFilter};
}

export function isGetSearchesFilter(value: unknown): value is GetSearchesFilter {
    return [isSearchesByQueryLike, isSearchesByResultLike, isNoSearchesFilter].some((typePredicate) => typePredicate(value));
}

export function isSearchesByQueryLike(value: unknown): value is SearchesByQueryLike {
    return svt.isInterface<SearchesByQueryLike>(value, {type: GetSearchesFilterTag.SearchesByQueryLike, data: svt.isString});
}

export function isSearchesByResultLike(value: unknown): value is SearchesByResultLike {
    return svt.isInterface<SearchesByResultLike>(value, {type: GetSearchesFilterTag.SearchesByResultLike, data: svt.isString});
}

export function isNoSearchesFilter(value: unknown): value is NoSearchesFilter {
    return svt.isInterface<NoSearchesFilter>(value, {type: GetSearchesFilterTag.NoSearchesFilter});
}

export function validateGetSearchesFilter(value: unknown): svt.ValidationResult<GetSearchesFilter> {
    return svt.validateWithTypeTag<GetSearchesFilter>(value, {[GetSearchesFilterTag.SearchesByQueryLike]: validateSearchesByQueryLike, [GetSearchesFilterTag.SearchesByResultLike]: validateSearchesByResultLike, [GetSearchesFilterTag.NoSearchesFilter]: validateNoSearchesFilter}, "type");
}

export function validateSearchesByQueryLike(value: unknown): svt.ValidationResult<SearchesByQueryLike> {
    return svt.validate<SearchesByQueryLike>(value, {type: GetSearchesFilterTag.SearchesByQueryLike, data: svt.validateString});
}

export function validateSearchesByResultLike(value: unknown): svt.ValidationResult<SearchesByResultLike> {
    return svt.validate<SearchesByResultLike>(value, {type: GetSearchesFilterTag.SearchesByResultLike, data: svt.validateString});
}

export function validateNoSearchesFilter(value: unknown): svt.ValidationResult<NoSearchesFilter> {
    return svt.validate<NoSearchesFilter>(value, {type: GetSearchesFilterTag.NoSearchesFilter});
}

export type SearchesParameters = {
    filters: GetSearchesFilter[];
};

export function isSearchesParameters(value: unknown): value is SearchesParameters {
    return svt.isInterface<SearchesParameters>(value, {filters: svt.arrayOf(isGetSearchesFilter)});
}

export function validateSearchesParameters(value: unknown): svt.ValidationResult<SearchesParameters> {
    return svt.validate<SearchesParameters>(value, {filters: svt.validateArray(validateGetSearchesFilter)});
}

export enum StillSize {
    w92 = "w92",
    w185 = "w185",
    w300 = "w300",
    h632 = "h632",
    original = "original",
}

export function isStillSize(value: unknown): value is StillSize {
    return [StillSize.w92, StillSize.w185, StillSize.w300, StillSize.h632, StillSize.original].some((v) => v === value);
}

export function validateStillSize(value: unknown): svt.ValidationResult<StillSize> {
    return svt.validateOneOfLiterals<StillSize>(value, [StillSize.w92, StillSize.w185, StillSize.w300, StillSize.h632, StillSize.original]);
}