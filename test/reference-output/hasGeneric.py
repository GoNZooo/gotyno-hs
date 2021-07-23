import json
import typing
from dataclasses import dataclass
from gotyno_validation import validation
from gotyno_validation import encoding

from . import external
from . import other

T = typing.TypeVar('T')
E = typing.TypeVar('E')
class Result(typing.Generic[T, E]):
    @staticmethod
    def validate(validate_T: validation.Validator[T], validate_E: validation.Validator[E]) -> validation.Validator['Result[T, E]']:
        def validate_ResultTE(value: validation.Unknown) -> validation.ValidationResult['Result[T, E]']:
            return validation.validate_with_type_tags(value, 'type', {'Success': Success.validate(validate_T), 'Failure': Failure.validate(validate_E)})
        return validate_ResultTE

    @staticmethod
    def decode(string: typing.Union[str, bytes], validate_T: validation.Validator[T], validate_E: validation.Validator[E]) -> validation.ValidationResult['Result[T, E]']:
        return validation.validate_from_string(string, Result.validate(validate_T, validate_E))

    def to_json(self) -> typing.Dict[str, typing.Any]:
        raise NotImplementedError('`to_json` is not implemented for base class `Result`')

    def encode(self) -> str:
        raise NotImplementedError('`encode` is not implemented for base class `Result`')

@dataclass(frozen=True)
class Success(Result[T, E]):
    data: T

    @staticmethod
    def validate(validate_T: validation.Validator[T]) -> validation.Validator['Success[T]']:
        def validate_SuccessT(value: validation.Unknown) -> validation.ValidationResult['Success[T]']:
            return validation.validate_with_type_tag(value, 'type', 'Success', {'data': validate_T}, Success)
        return validate_SuccessT

    @staticmethod
    def decode(string: typing.Union[str, bytes], validate_T: validation.Validator[T]) -> validation.ValidationResult['Success[T]']:
        return validation.validate_from_string(string, Success.validate(validate_T))

    def to_json(self, T_to_json: encoding.ToJSON[T]) -> typing.Dict[str, typing.Any]:
        return {'type': 'Success', 'data': T_to_json(self.data)}

    def encode(self, T_to_json: encoding.ToJSON[T]) -> str:
        return json.dumps(self.to_json(T_to_json))

@dataclass(frozen=True)
class Failure(Result[T, E]):
    data: E

    @staticmethod
    def validate(validate_E: validation.Validator[E]) -> validation.Validator['Failure[E]']:
        def validate_FailureE(value: validation.Unknown) -> validation.ValidationResult['Failure[E]']:
            return validation.validate_with_type_tag(value, 'type', 'Failure', {'data': validate_E}, Failure)
        return validate_FailureE

    @staticmethod
    def decode(string: typing.Union[str, bytes], validate_E: validation.Validator[E]) -> validation.ValidationResult['Failure[E]']:
        return validation.validate_from_string(string, Failure.validate(validate_E))

    def to_json(self, E_to_json: encoding.ToJSON[E]) -> typing.Dict[str, typing.Any]:
        return {'type': 'Failure', 'data': E_to_json(self.data)}

    def encode(self, E_to_json: encoding.ToJSON[E]) -> str:
        return json.dumps(self.to_json(E_to_json))

T = typing.TypeVar('T')
@dataclass(frozen=True)
class Holder(typing.Generic[T]):
    value: T

    @staticmethod
    def validate(validate_T: validation.Validator[T]) -> validation.Validator['Holder[T]']:
        def validate_HolderT(value: validation.Unknown) -> validation.ValidationResult['Holder[T]']:
            return validation.validate_interface(value, {'value': validate_T}, Holder)
        return validate_HolderT

    @staticmethod
    def decode(string: typing.Union[str, bytes], validate_T: validation.Validator[T]) -> validation.ValidationResult['Holder[T]']:
        return validation.validate_from_string(string, Holder.validate(validate_T))

    def to_json(self, T_to_json: encoding.ToJSON[T]) -> typing.Dict[str, typing.Any]:
        return {'value': T_to_json(self.value)}

    def encode(self, T_to_json: encoding.ToJSON[T]) -> str:
        return json.dumps(self.to_json(T_to_json))

T = typing.TypeVar('T')
@dataclass(frozen=True)
class MaybeHolder(typing.Generic[T]):
    value: external.Option[T]
    otherValue: other.Plain

    @staticmethod
    def validate(validate_T: validation.Validator[T]) -> validation.Validator['MaybeHolder[T]']:
        def validate_MaybeHolderT(value: validation.Unknown) -> validation.ValidationResult['MaybeHolder[T]']:
            return validation.validate_interface(value, {'value': external.Option.validate(validate_T), 'otherValue': other.Plain.validate}, MaybeHolder)
        return validate_MaybeHolderT

    @staticmethod
    def decode(string: typing.Union[str, bytes], validate_T: validation.Validator[T]) -> validation.ValidationResult['MaybeHolder[T]']:
        return validation.validate_from_string(string, MaybeHolder.validate(validate_T))

    def to_json(self, T_to_json: encoding.ToJSON[T]) -> typing.Dict[str, typing.Any]:
        return {'value': external.Option.to_json(T_to_json)(self.value), 'otherValue': other.Plain.to_json(self.otherValue)}

    def encode(self, T_to_json: encoding.ToJSON[T]) -> str:
        return json.dumps(self.to_json(T_to_json))

T = typing.TypeVar('T')
class HasGenericEvent(typing.Generic[T]):
    @staticmethod
    def validate(validate_T: validation.Validator[T]) -> validation.Validator['HasGenericEvent[T]']:
        def validate_HasGenericEventT(value: validation.Unknown) -> validation.ValidationResult['HasGenericEvent[T]']:
            return validation.validate_with_type_tags(value, 'type', {'PlainEvent': PlainEvent.validate, 'GenericEvent': GenericEvent.validate(validate_T)})
        return validate_HasGenericEventT

    @staticmethod
    def decode(string: typing.Union[str, bytes], validate_T: validation.Validator[T]) -> validation.ValidationResult['HasGenericEvent[T]']:
        return validation.validate_from_string(string, HasGenericEvent.validate(validate_T))

    def to_json(self) -> typing.Dict[str, typing.Any]:
        raise NotImplementedError('`to_json` is not implemented for base class `HasGenericEvent`')

    def encode(self) -> str:
        raise NotImplementedError('`encode` is not implemented for base class `HasGenericEvent`')

@dataclass(frozen=True)
class PlainEvent(HasGenericEvent[T]):
    data: other.Plain

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['PlainEvent']:
        return validation.validate_with_type_tag(value, 'type', 'PlainEvent', {'data': other.Plain.validate}, PlainEvent)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['PlainEvent']:
        return validation.validate_from_string(string, PlainEvent.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'type': 'PlainEvent', 'data': self.data.to_json()}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass(frozen=True)
class GenericEvent(HasGenericEvent[T]):
    data: external.Option[T]

    @staticmethod
    def validate(validate_T: validation.Validator[T]) -> validation.Validator['GenericEvent[T]']:
        def validate_GenericEventT(value: validation.Unknown) -> validation.ValidationResult['GenericEvent[T]']:
            return validation.validate_with_type_tag(value, 'type', 'GenericEvent', {'data': external.Option.validate(validate_T)}, GenericEvent)
        return validate_GenericEventT

    @staticmethod
    def decode(string: typing.Union[str, bytes], validate_T: validation.Validator[T]) -> validation.ValidationResult['GenericEvent[T]']:
        return validation.validate_from_string(string, GenericEvent.validate(validate_T))

    def to_json(self, T_to_json: encoding.ToJSON[T]) -> typing.Dict[str, typing.Any]:
        return {'type': 'GenericEvent', 'data': self.data.to_json()}

    def encode(self, T_to_json: encoding.ToJSON[T]) -> str:
        return json.dumps(self.to_json(T_to_json))