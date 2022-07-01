# pylint: disable=too-many-lines, invalid-name, line-too-long, missing-module-docstring, missing-class-docstring, missing-function-docstring
import enum
import json
import typing
from dataclasses import dataclass
from gotyno_validation import encoding, validation

from . import supportTypes

@dataclass(frozen=True)
class ValidationError:
    data: typing.Union[supportTypes.StringMap['ValidationError'], str]

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['ValidationError']:
        return validation.validate_one_of_with_constructor(value, [supportTypes.StringMap.validate(ValidationError.validate), validation.validate_string], ValidationError)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['ValidationError']:
        return validation.validate_from_string(string, ValidationError.validate)

    def to_json(self) -> typing.Any:
        return encoding.one_of_to_json(self.data, {supportTypes.StringMap: supportTypes.StringMap.to_json, str: encoding.basic_to_json})

    def encode(self) -> str:
        return json.dumps(self.to_json())