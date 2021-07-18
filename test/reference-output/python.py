import json
import typing
from dataclasses import dataclass
import validation as v

@dataclass(frozen=True)
class SomeType:
    type: typing.Literal['SomeType']
    some_field: str
    some_other_field: int
    maybe_some_field: typing.Optional[str]

    @staticmethod
    def validate(value: v.Unknown) -> v.ValidationResult['SomeType']:
        return v.validate_interface(value, {'type': v.validate_literal('SomeType'), 'some_field': v.validate_string, 'some_other_field': v.validate_int, 'maybe_some_field': v.validate_optional(v.validate_string)})

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> v.ValidationResult['SomeType']:
        return v.validate_from_string(string, SomeType.validate)

    def encode(self) -> str:
        return json.dumps(self.__dict__)

T = typing.TypeVar('T')
@dataclass(frozen=True)
class Holder(typing.Generic[T]):
    value: T

    @staticmethod
    def validate(validate_T: v.Validator[T]) -> v.Validator['Holder[T]']:
        def validate_HolderT(value: v.Unknown) -> v.ValidationResult['Holder[T]']:
            return v.validate_interface(value, {'value': validate_T})
        return validate_HolderT

    @staticmethod
    def decode(string: typing.Union[str, bytes], validate_T: v.Validator[T]) -> v.ValidationResult['Holder[T]']:
        return v.validate_from_string(string, Holder.validate(validate_T))

    def encode(self) -> str:
        return json.dumps(self.__dict__)