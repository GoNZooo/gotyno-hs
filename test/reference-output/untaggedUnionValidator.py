import enum
import json
import typing
from dataclasses import dataclass
from gotyno_validation import encoding, validation

from . import controlTypes

@dataclass(frozen=True)
class SplitName:
    firstName: str
    lastName: str

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['SplitName']:
        return validation.validate_interface(value, {'firstName': validation.validate_string, 'lastName': validation.validate_string}, SplitName)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['SplitName']:
        return validation.validate_from_string(string, SplitName.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'firstName': self.firstName, 'lastName': self.lastName}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass(frozen=True)
class FullName:
    fullName: str

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['FullName']:
        return validation.validate_interface(value, {'fullName': validation.validate_string}, FullName)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['FullName']:
        return validation.validate_from_string(string, FullName.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'fullName': self.fullName}

    def encode(self) -> str:
        return json.dumps(self.to_json())

Name = typing.Union[SplitName, FullName]
class NameInterface:
    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['Name']:
        return validation.validate_one_of(value, [SplitName.validate, FullName.validate])

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['Name']:
        return validation.validate_from_string(string, NameInterface.validate)

    @staticmethod
    def to_json(value) -> typing.Any:
        return encoding.one_of_to_json(value, {SplitName: SplitName.to_json, FullName: FullName.to_json})

    @staticmethod
    def encode(value) -> str:
        return json.dumps(value.to_json())

@dataclass(frozen=True)
class CompanyPerson:
    name: controlTypes.Option[Name]
    title: controlTypes.Option[str]
    isCurrent: controlTypes.Option[bool]

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['CompanyPerson']:
        return validation.validate_interface(value, {'name': controlTypes.Option.validate(NameInterface.validate), 'title': controlTypes.Option.validate(validation.validate_string), 'isCurrent': controlTypes.Option.validate(validation.validate_bool)}, CompanyPerson)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['CompanyPerson']:
        return validation.validate_from_string(string, CompanyPerson.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'name': self.name.to_json(), 'title': self.title.to_json(), 'isCurrent': self.isCurrent.to_json()}

    def encode(self) -> str:
        return json.dumps(self.to_json())