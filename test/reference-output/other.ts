import * as svt from "simple-validation-tools";

export interface Plain {
  type: "Plain";
  field1: string;
}

export function isPlain(value: unknown): value is Plain {
  return svt.isInterface<Plain>(value, {type: "Plain", field1: svt.isString})
}

export function validatePlain(value: unknown): svt.ValidationResult<Plain> {
  return svt.validate<Plain>(value, {type: "Plain", field1: svt.validateString})
}
