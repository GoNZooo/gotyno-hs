import json
import typing
from dataclasses import dataclass
from gotyno_validation import encoding, validation

from . import basic

@dataclass(frozen=True)
class UsesImport:
    type: typing.Literal['UsesImport']
    recruiter: basic.Recruiter

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['UsesImport']:
        return validation.validate_interface(value, {'type': validation.validate_literal('UsesImport'), 'recruiter': basic.Recruiter.validate}, UsesImport)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['UsesImport']:
        return validation.validate_from_string(string, UsesImport.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'type': 'UsesImport', 'recruiter': basic.Recruiter.to_json(self.recruiter)}

    def encode(self) -> str:
        return json.dumps(self.to_json())

T = typing.TypeVar('T')
@dataclass(frozen=True)
class HoldsSomething(typing.Generic[T]):
    holdingField: T

    @staticmethod
    def validate(validate_T: validation.Validator[T]) -> validation.Validator['HoldsSomething[T]']:
        def validate_HoldsSomethingT(value: validation.Unknown) -> validation.ValidationResult['HoldsSomething[T]']:
            return validation.validate_interface(value, {'holdingField': validate_T}, HoldsSomething)
        return validate_HoldsSomethingT

    @staticmethod
    def decode(string: typing.Union[str, bytes], validate_T: validation.Validator[T]) -> validation.ValidationResult['HoldsSomething[T]']:
        return validation.validate_from_string(string, HoldsSomething.validate(validate_T))

    def to_json(self, T_to_json: encoding.ToJSON[T]) -> typing.Dict[str, typing.Any]:
        return {'holdingField': T_to_json(self.holdingField)}

    def encode(self, T_to_json: encoding.ToJSON[T]) -> str:
        return json.dumps(self.to_json(T_to_json))

@dataclass(frozen=True)
class StructureUsingImport:
    event: basic.Event

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['StructureUsingImport']:
        return validation.validate_interface(value, {'event': basic.Event.validate}, StructureUsingImport)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['StructureUsingImport']:
        return validation.validate_from_string(string, StructureUsingImport.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'event': basic.Event.to_json(self.event)}

    def encode(self) -> str:
        return json.dumps(self.to_json())

class UnionUsingImport:
    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['UnionUsingImport']:
        return validation.validate_with_type_tags(value, 'type', {'CoolEvent': CoolEvent.validate, 'Other': Other.validate})

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['UnionUsingImport']:
        return validation.validate_from_string(string, UnionUsingImport.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        raise NotImplementedError('`to_json` is not implemented for base class `UnionUsingImport`')

    def encode(self) -> str:
        raise NotImplementedError('`encode` is not implemented for base class `UnionUsingImport`')

@dataclass(frozen=True)
class CoolEvent(UnionUsingImport):
    data: basic.Event

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['CoolEvent']:
        return validation.validate_with_type_tag(value, 'type', 'CoolEvent', {'data': basic.Event.validate}, CoolEvent)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['CoolEvent']:
        return validation.validate_from_string(string, CoolEvent.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'type': 'CoolEvent', 'data': self.data.to_json()}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass(frozen=True)
class Other(UnionUsingImport):
    data: basic.Person

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['Other']:
        return validation.validate_with_type_tag(value, 'type', 'Other', {'data': basic.Person.validate}, Other)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['Other']:
        return validation.validate_from_string(string, Other.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'type': 'Other', 'data': self.data.to_json()}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass(frozen=True)
class AllConcrete:
    field: HoldsSomething[basic.Either[basic.Maybe[StructureUsingImport], UnionUsingImport]]

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['AllConcrete']:
        return validation.validate_interface(value, {'field': (HoldsSomething.validate (basic.Either.validate (basic.Maybe.validate StructureUsingImport.validate) UnionUsingImport.validate))}, AllConcrete)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['AllConcrete']:
        return validation.validate_from_string(string, AllConcrete.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'field': HoldsSomething.to_json(basic.Either.to_json(basic.Maybe.to_json(StructureUsingImport.to_json) UnionUsingImport.to_json))(self.field)}

    def encode(self) -> str:
        return json.dumps(self.to_json())