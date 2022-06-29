import enum
import json
import typing
from dataclasses import dataclass
from gotyno_validation import encoding, validation

T = typing.TypeVar('T')
@dataclass(frozen=True)
class ExceptionPayload(typing.Generic[T]):
    notificationId: str
    input: T
    errorName: str
    errorMessage: str
    errorStack: typing.Optional[str]

    @staticmethod
    def validate(validate_T: validation.Validator[T]) -> validation.Validator['ExceptionPayload[T]']:
        def validate_ExceptionPayloadT(value: validation.Unknown) -> validation.ValidationResult['ExceptionPayload[T]']:
            return validation.validate_interface(value, {'notificationId': validation.validate_string, 'input': validate_T, 'errorName': validation.validate_string, 'errorMessage': validation.validate_string, 'errorStack': validation.validate_optional(validation.validate_string)}, ExceptionPayload)
        return validate_ExceptionPayloadT

    @staticmethod
    def decode(string: typing.Union[str, bytes], validate_T: validation.Validator[T]) -> validation.ValidationResult['ExceptionPayload[T]']:
        return validation.validate_from_string(string, ExceptionPayload.validate(validate_T))

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'notificationId': self.notificationId, 'input': encoding.general_to_json(self.input), 'errorName': self.errorName, 'errorMessage': self.errorMessage, 'errorStack': encoding.optional_to_json(encoding.basic_to_json)(self.errorStack)}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass(frozen=True)
class ExceptionS3Reference:
    notificationId: str
    bucket: str
    key: str

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['ExceptionS3Reference']:
        return validation.validate_interface(value, {'notificationId': validation.validate_string, 'bucket': validation.validate_string, 'key': validation.validate_string}, ExceptionS3Reference)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['ExceptionS3Reference']:
        return validation.validate_from_string(string, ExceptionS3Reference.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'notificationId': self.notificationId, 'bucket': self.bucket, 'key': self.key}

    def encode(self) -> str:
        return json.dumps(self.to_json())

T = typing.TypeVar('T')
class ExceptionNotification(typing.Generic[T]):
    @staticmethod
    def validate(validate_T: validation.Validator[T]) -> validation.Validator['ExceptionNotification[T]']:
        def validate_ExceptionNotificationT(value: validation.Unknown) -> validation.ValidationResult['ExceptionNotification[T]']:
            return validation.validate_with_type_tags(value, 'type', {'ExceptionNotificationPayload': ExceptionNotificationPayload.validate(validate_T), 'ExceptionNotificationS3Reference': ExceptionNotificationS3Reference.validate})
        return validate_ExceptionNotificationT

    @staticmethod
    def decode(string: typing.Union[str, bytes], validate_T: validation.Validator[T]) -> validation.ValidationResult['ExceptionNotification[T]']:
        return validation.validate_from_string(string, ExceptionNotification.validate(validate_T))

    def to_json(self) -> typing.Dict[str, typing.Any]:
        raise NotImplementedError('`to_json` is not implemented for base class `ExceptionNotification`')

    def encode(self) -> str:
        raise NotImplementedError('`encode` is not implemented for base class `ExceptionNotification`')

@dataclass(frozen=True)
class ExceptionNotificationPayload(ExceptionNotification[T]):
    data: ExceptionPayload[T]

    @staticmethod
    def validate(validate_T: validation.Validator[T]) -> validation.Validator['ExceptionNotificationPayload[T]']:
        def validate_ExceptionNotificationPayloadT(value: validation.Unknown) -> validation.ValidationResult['ExceptionNotificationPayload[T]']:
            return validation.validate_with_type_tag(value, 'type', 'ExceptionNotificationPayload', {'data': ExceptionPayload.validate(validate_T)}, ExceptionNotificationPayload)
        return validate_ExceptionNotificationPayloadT

    @staticmethod
    def decode(string: typing.Union[str, bytes], validate_T: validation.Validator[T]) -> validation.ValidationResult['ExceptionNotificationPayload[T]']:
        return validation.validate_from_string(string, ExceptionNotificationPayload.validate(validate_T))

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'type': 'ExceptionNotificationPayload', 'data': self.data.to_json()}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass(frozen=True)
class ExceptionNotificationS3Reference(ExceptionNotification[T]):
    data: ExceptionS3Reference

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['ExceptionNotificationS3Reference']:
        return validation.validate_with_type_tag(value, 'type', 'ExceptionNotificationS3Reference', {'data': ExceptionS3Reference.validate}, ExceptionNotificationS3Reference)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['ExceptionNotificationS3Reference']:
        return validation.validate_from_string(string, ExceptionNotificationS3Reference.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'type': 'ExceptionNotificationS3Reference', 'data': self.data.to_json()}

    def encode(self) -> str:
        return json.dumps(self.to_json())