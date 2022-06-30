import enum
import json
import typing
from dataclasses import dataclass
from gotyno_validation import encoding, validation

@dataclass(frozen=True)
class AStruct:
    from_: int

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['AStruct']:
        return validation.validate_interface(value, {'from': validation.validate_int}, AStruct)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['AStruct']:
        return validation.validate_from_string(string, AStruct.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'from': self.from_}

    def encode(self) -> str:
        return json.dumps(self.to_json())