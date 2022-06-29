import enum
import json
import typing
from dataclasses import dataclass
from gotyno_validation import encoding, validation

from . import supportTypes

ValidationError = typing.Union[supportTypes.StringMap['ValidationError'], str]
class ValidationErrorInterface:
    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['ValidationError']:
        return validation.validate_one_of(value, [supportTypes.StringMap.validate(ValidationError.validate), validation.validate_string])

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['ValidationError']:
        return validation.validate_from_string(string, ValidationErrorInterface.validate)

    @staticmethod
    def to_json(value) -> typing.Any:
        return encoding.one_of_to_json(value, {supportTypes.StringMap: supportTypes.StringMap.to_json(ValidationError.to_json), str: encoding.basic_to_json})

    @staticmethod
    def encode(value) -> str:
        return json.dumps(value.to_json())