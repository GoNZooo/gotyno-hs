import json
import typing
from dataclasses import dataclass
from gotyno_validation import validation
from gotyno_validation import encoding

@dataclass(frozen=True)
class SomeType:
    type: typing.Literal['SomeType']
    some_field: str
    some_other_field: int
    maybe_some_field: typing.Optional[str]

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['SomeType']:
        return validation.validate_interface(value, {'type': validation.validate_literal('SomeType'), 'some_field': validation.validate_string, 'some_other_field': validation.validate_int, 'maybe_some_field': validation.validate_optional(validation.validate_string)})

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['SomeType']:
        return validation.validate_from_string(string, SomeType.validate)

    def encode(self) -> str:
        return json.dumps({'type': 'SomeType', 'some_field': self.some_field, 'some_other_field': self.some_other_field, 'maybe_some_field': encoding.encode_optional(encoding.encode_basic)(self.maybe_some_field)})

T = typing.TypeVar('T')
@dataclass(frozen=True)
class Holder(typing.Generic[T]):
    value: T

    @staticmethod
    def validate(validate_T: validation.Validator[T]) -> validation.Validator['Holder[T]']:
        def validate_HolderT(value: validation.Unknown) -> validation.ValidationResult['Holder[T]']:
            return validation.validate_interface(value, {'value': validate_T})
        return validate_HolderT

    @staticmethod
    def decode(string: typing.Union[str, bytes], validate_T: validation.Validator[T]) -> validation.ValidationResult['Holder[T]']:
        return validation.validate_from_string(string, Holder.validate(validate_T))

    def encode(self, encode_T: encoding.Encoder[T]) -> str:
        return json.dumps({'value': encode_T(self.value)})

class Event:
    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['Event']:
        return validation.validate_with_type_tags(value, 'type', {'Notification': Notification.validate, 'Launch': Launch.validate, 'AnotherEvent': AnotherEvent.validate})

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['Event']:
        return validation.validate_from_string(string, Event.validate)

@dataclass(frozen=True)
class Notification(Event):
    data: str

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['Notification']:
        return validation.validate_with_type_tag(value, 'type', 'Notification', {'data': validation.validate_string}, Notification)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['Notification']:
        return validation.validate_from_string(string, Notification.validate)

    def encode(self) -> str:
        return json.dumps({'type': 'Notification', 'data': self.data})

@dataclass(frozen=True)
class Launch(Event):
    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['Launch']:
        return validation.validate_with_type_tag(value, 'type', 'Launch', {}, Launch)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['Launch']:
        return validation.validate_from_string(string, Launch.validate)

    def encode(self) -> str:
        return json.dumps({'type': 'Launch'})

@dataclass(frozen=True)
class AnotherEvent(Event):
    data: SomeType

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['AnotherEvent']:
        return validation.validate_with_type_tag(value, 'type', 'AnotherEvent', {'data': SomeType.validate}, AnotherEvent)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['AnotherEvent']:
        return validation.validate_from_string(string, AnotherEvent.validate)

    def encode(self) -> str:
        return json.dumps({'type': 'AnotherEvent', 'data': self.data.encode()})

T = typing.TypeVar('T')
class Possibly(typing.Generic[T]):
    @staticmethod
    def validate(validate_T: validation.Validator[T]) -> validation.Validator['Possibly[T]']:
        def validate_PossiblyT(value: validation.Unknown) -> validation.ValidationResult['Possibly[T]']:
            return validation.validate_with_type_tags(value, 'type', {'NotReally': NotReally.validate, 'Definitely': Definitely.validate(validate_T)})
        return validate_PossiblyT

    @staticmethod
    def decode(string: typing.Union[str, bytes], validate_T: validation.Validator[T]) -> validation.ValidationResult['Possibly[T]']:
        return validation.validate_from_string(string, Possibly.validate(validate_T))

@dataclass(frozen=True)
class NotReally(Possibly[T]):
    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['NotReally']:
        return validation.validate_with_type_tag(value, 'type', 'NotReally', {}, NotReally)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['NotReally']:
        return validation.validate_from_string(string, NotReally.validate)

    def encode(self) -> str:
        return json.dumps({'type': 'NotReally'})

@dataclass(frozen=True)
class Definitely(Possibly[T]):
    data: T

    @staticmethod
    def validate(validate_T: validation.Validator[T]) -> validation.Validator['Definitely[T]']:
        def validate_DefinitelyT(value: validation.Unknown) -> validation.ValidationResult['Definitely[T]']:
            return validation.validate_with_type_tag(value, 'type', 'Definitely', {'data': validate_T}, Definitely)
        return validate_DefinitelyT

    @staticmethod
    def decode(string: typing.Union[str, bytes], validate_T: validation.Validator[T]) -> validation.ValidationResult['Definitely[T]']:
        return validation.validate_from_string(string, Definitely.validate(validate_T))

    def encode(self, encode_T: encoding.Encoder[T]) -> str:
        return json.dumps({'type': 'Definitely', 'data': encode_T(self.data)})