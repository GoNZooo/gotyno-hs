import enum
import json
import typing
from dataclasses import dataclass
from gotyno_validation import encoding, validation

T = typing.TypeVar('T')
class Option(typing.Generic[T]):
    @staticmethod
    def validate(validate_T: validation.Validator[T]) -> validation.Validator['Option[T]']:
        def validate_OptionT(value: validation.Unknown) -> validation.ValidationResult['Option[T]']:
            return validation.validate_with_type_tags(value, 'type', {'None': None_.validate, 'Some': Some.validate(validate_T)})
        return validate_OptionT

    @staticmethod
    def decode(string: typing.Union[str, bytes], validate_T: validation.Validator[T]) -> validation.ValidationResult['Option[T]']:
        return validation.validate_from_string(string, Option.validate(validate_T))

    def to_json(self) -> typing.Dict[str, typing.Any]:
        raise NotImplementedError('`to_json` is not implemented for base class `Option`')

    def encode(self) -> str:
        raise NotImplementedError('`encode` is not implemented for base class `Option`')

@dataclass(frozen=True)
class None_(Option[T]):
    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['None_']:
        return validation.validate_with_type_tag(value, 'type', 'None', {}, None_)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['None_']:
        return validation.validate_from_string(string, None_.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'type': 'None'}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass(frozen=True)
class Some(Option[T]):
    data: T

    @staticmethod
    def validate(validate_T: validation.Validator[T]) -> validation.Validator['Some[T]']:
        def validate_SomeT(value: validation.Unknown) -> validation.ValidationResult['Some[T]']:
            return validation.validate_with_type_tag(value, 'type', 'Some', {'data': validate_T}, Some)
        return validate_SomeT

    @staticmethod
    def decode(string: typing.Union[str, bytes], validate_T: validation.Validator[T]) -> validation.ValidationResult['Some[T]']:
        return validation.validate_from_string(string, Some.validate(validate_T))

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'type': 'Some', 'data': encoding.general_to_json(self.data)}

    def encode(self) -> str:
        return json.dumps(self.to_json())