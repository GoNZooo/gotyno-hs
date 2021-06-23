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