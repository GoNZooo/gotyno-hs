import enum
import json
import typing
from dataclasses import dataclass
from gotyno_validation import encoding, validation

@dataclass(frozen=True)
class Recruiter:
    type: typing.Literal['Recruiter']
    Name: str
    emails: typing.List[typing.Optional[str]]
    recruiter: typing.Optional['Recruiter']
    created: int

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['Recruiter']:
        return validation.validate_interface(value, {'type': validation.validate_literal('Recruiter'), 'Name': validation.validate_string, 'emails': validation.validate_list(validation.validate_optional(validation.validate_string)), 'recruiter': validation.validate_optional(Recruiter.decode), 'created': validation.validate_bigint}, Recruiter)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['Recruiter']:
        return validation.validate_from_string(string, Recruiter.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'type': 'Recruiter', 'Name': self.Name, 'emails': encoding.list_to_json((encoding.optional_to_json(encoding.basic_to_json)))(self.emails), 'recruiter': encoding.optional_to_json((Recruiter.to_json))(self.recruiter), 'created': encoding.bigint_to_json(self.created)}

    def encode(self) -> str:
        return json.dumps(self.to_json())

class GetSearchesFilter:
    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['GetSearchesFilter']:
        return validation.validate_with_type_tags(value, 'type', {'SearchesByQueryLike': SearchesByQueryLike.validate, 'SearchesByResultLike': SearchesByResultLike.validate, 'NoSearchesFilter': NoSearchesFilter.validate})

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['GetSearchesFilter']:
        return validation.validate_from_string(string, GetSearchesFilter.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        raise NotImplementedError('`to_json` is not implemented for base class `GetSearchesFilter`')

    def encode(self) -> str:
        raise NotImplementedError('`encode` is not implemented for base class `GetSearchesFilter`')

@dataclass(frozen=True)
class SearchesByQueryLike(GetSearchesFilter):
    data: str

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['SearchesByQueryLike']:
        return validation.validate_with_type_tag(value, 'type', 'SearchesByQueryLike', {'data': validation.validate_string}, SearchesByQueryLike)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['SearchesByQueryLike']:
        return validation.validate_from_string(string, SearchesByQueryLike.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'type': 'SearchesByQueryLike', 'data': self.data}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass(frozen=True)
class SearchesByResultLike(GetSearchesFilter):
    data: str

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['SearchesByResultLike']:
        return validation.validate_with_type_tag(value, 'type', 'SearchesByResultLike', {'data': validation.validate_string}, SearchesByResultLike)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['SearchesByResultLike']:
        return validation.validate_from_string(string, SearchesByResultLike.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'type': 'SearchesByResultLike', 'data': self.data}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass(frozen=True)
class NoSearchesFilter(GetSearchesFilter):
    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['NoSearchesFilter']:
        return validation.validate_with_type_tag(value, 'type', 'NoSearchesFilter', {}, NoSearchesFilter)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['NoSearchesFilter']:
        return validation.validate_from_string(string, NoSearchesFilter.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'type': 'NoSearchesFilter'}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass(frozen=True)
class SearchesParameters:
    filters: typing.List[GetSearchesFilter]

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['SearchesParameters']:
        return validation.validate_interface(value, {'filters': validation.validate_list(GetSearchesFilter.validate)}, SearchesParameters)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['SearchesParameters']:
        return validation.validate_from_string(string, SearchesParameters.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'filters': encoding.list_to_json(GetSearchesFilter.to_json)(self.filters)}

    def encode(self) -> str:
        return json.dumps(self.to_json())

class StillSize(enum.Enum):
    w92 = "w92"
    w185 = "w185"
    w300 = "w300"
    h632 = "h632"
    original = "original"

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['StillSize']:
        return validation.validate_enumeration_member(value, StillSize)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['StillSize']:
        return validation.validate_from_string(string, StillSize.validate)

    def to_json(self) -> typing.Any:
        return self.value

    def encode(self) -> str:
        return str(self.value)

@dataclass(frozen=True)
class LogInData:
    username: str
    password: str

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['LogInData']:
        return validation.validate_interface(value, {'username': validation.validate_string, 'password': validation.validate_string}, LogInData)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['LogInData']:
        return validation.validate_from_string(string, LogInData.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'username': self.username, 'password': self.password}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass(frozen=True)
class UserId:
    value: str

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['UserId']:
        return validation.validate_interface(value, {'value': validation.validate_string}, UserId)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['UserId']:
        return validation.validate_from_string(string, UserId.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'value': self.value}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass(frozen=True)
class Channel:
    name: str
    private: bool

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['Channel']:
        return validation.validate_interface(value, {'name': validation.validate_string, 'private': validation.validate_bool}, Channel)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['Channel']:
        return validation.validate_from_string(string, Channel.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'name': self.name, 'private': self.private}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass(frozen=True)
class Email:
    value: str

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['Email']:
        return validation.validate_interface(value, {'value': validation.validate_string}, Email)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['Email']:
        return validation.validate_from_string(string, Email.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'value': self.value}

    def encode(self) -> str:
        return json.dumps(self.to_json())

class Event:
    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['Event']:
        return validation.validate_with_type_tags(value, 'type', {'LogIn': LogIn.validate, 'LogOut': LogOut.validate, 'JoinChannels': JoinChannels.validate, 'SetEmails': SetEmails.validate})

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['Event']:
        return validation.validate_from_string(string, Event.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        raise NotImplementedError('`to_json` is not implemented for base class `Event`')

    def encode(self) -> str:
        raise NotImplementedError('`encode` is not implemented for base class `Event`')

@dataclass(frozen=True)
class LogIn(Event):
    data: LogInData

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['LogIn']:
        return validation.validate_with_type_tag(value, 'type', 'LogIn', {'data': LogInData.validate}, LogIn)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['LogIn']:
        return validation.validate_from_string(string, LogIn.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'type': 'LogIn', 'data': self.data.to_json()}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass(frozen=True)
class LogOut(Event):
    data: UserId

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['LogOut']:
        return validation.validate_with_type_tag(value, 'type', 'LogOut', {'data': UserId.validate}, LogOut)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['LogOut']:
        return validation.validate_from_string(string, LogOut.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'type': 'LogOut', 'data': self.data.to_json()}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass(frozen=True)
class JoinChannels(Event):
    data: typing.List[Channel]

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['JoinChannels']:
        return validation.validate_with_type_tag(value, 'type', 'JoinChannels', {'data': validation.validate_list(Channel.validate)}, JoinChannels)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['JoinChannels']:
        return validation.validate_from_string(string, JoinChannels.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'type': 'JoinChannels', 'data': encoding.list_to_json(Channel.to_json)(self.data)}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass(frozen=True)
class SetEmails(Event):
    data: typing.List[Email]

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['SetEmails']:
        return validation.validate_with_type_tag(value, 'type', 'SetEmails', {'data': validation.validate_list(Email.validate)}, SetEmails)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['SetEmails']:
        return validation.validate_from_string(string, SetEmails.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'type': 'SetEmails', 'data': encoding.list_to_json(Email.to_json)(self.data)}

    def encode(self) -> str:
        return json.dumps(self.to_json())

T = typing.TypeVar('T')
class Maybe(typing.Generic[T]):
    @staticmethod
    def validate(validate_T: validation.Validator[T]) -> validation.Validator['Maybe[T]']:
        def validate_MaybeT(value: validation.Unknown) -> validation.ValidationResult['Maybe[T]']:
            return validation.validate_with_type_tags(value, 'type', {'Nothing': Nothing.validate, 'Just': Just.validate(validate_T)})
        return validate_MaybeT

    @staticmethod
    def decode(string: typing.Union[str, bytes], validate_T: validation.Validator[T]) -> validation.ValidationResult['Maybe[T]']:
        return validation.validate_from_string(string, Maybe.validate(validate_T))

    def to_json(self, T_to_json: encoding.ToJSON[T]) -> typing.Dict[str, typing.Any]:
        raise NotImplementedError('`to_json` is not implemented for base class `Maybe`')

    def encode(self) -> str:
        raise NotImplementedError('`encode` is not implemented for base class `Maybe`')

@dataclass(frozen=True)
class Nothing(Maybe[T]):
    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['Nothing']:
        return validation.validate_with_type_tag(value, 'type', 'Nothing', {}, Nothing)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['Nothing']:
        return validation.validate_from_string(string, Nothing.validate)

    def to_json(self, T_to_json: encoding.ToJSON[T]) -> typing.Dict[str, typing.Any]:
        return {'type': 'Nothing'}

    def encode(self, T_to_json: encoding.ToJSON[T]) -> str:
        return json.dumps(self.to_json(T_to_json))

@dataclass(frozen=True)
class Just(Maybe[T]):
    data: T

    @staticmethod
    def validate(validate_T: validation.Validator[T]) -> validation.Validator['Just[T]']:
        def validate_JustT(value: validation.Unknown) -> validation.ValidationResult['Just[T]']:
            return validation.validate_with_type_tag(value, 'type', 'Just', {'data': validate_T}, Just)
        return validate_JustT

    @staticmethod
    def decode(string: typing.Union[str, bytes], validate_T: validation.Validator[T]) -> validation.ValidationResult['Just[T]']:
        return validation.validate_from_string(string, Just.validate(validate_T))

    def to_json(self, T_to_json: encoding.ToJSON[T]) -> typing.Dict[str, typing.Any]:
        return {'type': 'Just', 'data': T_to_json(self.data)}

    def encode(self, T_to_json: encoding.ToJSON[T]) -> str:
        return json.dumps(self.to_json(T_to_json))

L = typing.TypeVar('L')
R = typing.TypeVar('R')
class Either(typing.Generic[L, R]):
    @staticmethod
    def validate(validate_L: validation.Validator[L], validate_R: validation.Validator[R]) -> validation.Validator['Either[L, R]']:
        def validate_EitherLR(value: validation.Unknown) -> validation.ValidationResult['Either[L, R]']:
            return validation.validate_with_type_tags(value, 'type', {'Left': Left.validate(validate_L), 'Right': Right.validate(validate_R)})
        return validate_EitherLR

    @staticmethod
    def decode(string: typing.Union[str, bytes], validate_L: validation.Validator[L], validate_R: validation.Validator[R]) -> validation.ValidationResult['Either[L, R]']:
        return validation.validate_from_string(string, Either.validate(validate_L, validate_R))

    def to_json(self, L_to_json: encoding.ToJSON[L],R_to_json: encoding.ToJSON[R]) -> typing.Dict[str, typing.Any]:
        raise NotImplementedError('`to_json` is not implemented for base class `Either`')

    def encode(self) -> str:
        raise NotImplementedError('`encode` is not implemented for base class `Either`')

@dataclass(frozen=True)
class Left(Either[L, R]):
    data: L

    @staticmethod
    def validate(validate_L: validation.Validator[L]) -> validation.Validator['Left[L]']:
        def validate_LeftL(value: validation.Unknown) -> validation.ValidationResult['Left[L]']:
            return validation.validate_with_type_tag(value, 'type', 'Left', {'data': validate_L}, Left)
        return validate_LeftL

    @staticmethod
    def decode(string: typing.Union[str, bytes], validate_L: validation.Validator[L]) -> validation.ValidationResult['Left[L]']:
        return validation.validate_from_string(string, Left.validate(validate_L))

    def to_json(self, L_to_json: encoding.ToJSON[L], R_to_json: encoding.ToJSON[R]) -> typing.Dict[str, typing.Any]:
        return {'type': 'Left', 'data': L_to_json(self.data)}

    def encode(self, L_to_json: encoding.ToJSON[L], R_to_json: encoding.ToJSON[R]) -> str:
        return json.dumps(self.to_json(L_to_json, R_to_json))

@dataclass(frozen=True)
class Right(Either[L, R]):
    data: R

    @staticmethod
    def validate(validate_R: validation.Validator[R]) -> validation.Validator['Right[R]']:
        def validate_RightR(value: validation.Unknown) -> validation.ValidationResult['Right[R]']:
            return validation.validate_with_type_tag(value, 'type', 'Right', {'data': validate_R}, Right)
        return validate_RightR

    @staticmethod
    def decode(string: typing.Union[str, bytes], validate_R: validation.Validator[R]) -> validation.ValidationResult['Right[R]']:
        return validation.validate_from_string(string, Right.validate(validate_R))

    def to_json(self, L_to_json: encoding.ToJSON[L], R_to_json: encoding.ToJSON[R]) -> typing.Dict[str, typing.Any]:
        return {'type': 'Right', 'data': R_to_json(self.data)}

    def encode(self, L_to_json: encoding.ToJSON[L], R_to_json: encoding.ToJSON[R]) -> str:
        return json.dumps(self.to_json(L_to_json, R_to_json))

@dataclass(frozen=True)
class Person:
    name: str
    age: int
    efficiency: float
    on_vacation: bool
    hobbies: typing.List[str]
    last_fifteen_comments: typing.List[str]
    recruiter: Recruiter
    spouse: Maybe['Person']

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['Person']:
        return validation.validate_interface(value, {'name': validation.validate_string, 'age': validation.validate_int, 'efficiency': validation.validate_float, 'on_vacation': validation.validate_bool, 'hobbies': validation.validate_list(validation.validate_string), 'last_fifteen_comments': validation.validate_list(validation.validate_string), 'recruiter': Recruiter.validate, 'spouse': Maybe.validate(Person.decode)}, Person)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['Person']:
        return validation.validate_from_string(string, Person.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'name': self.name, 'age': self.age, 'efficiency': self.efficiency, 'on_vacation': self.on_vacation, 'hobbies': encoding.list_to_json(encoding.basic_to_json)(self.hobbies), 'last_fifteen_comments': encoding.list_to_json(encoding.basic_to_json)(self.last_fifteen_comments), 'recruiter': Recruiter.to_json(self.recruiter), 'spouse': self.spouse.to_json(Person.to_json)}

    def encode(self) -> str:
        return json.dumps(self.to_json())

class EmbeddedEvent:
    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['EmbeddedEvent']:
        return validation.validate_with_type_tags(value, 'type', {'EmbeddedLogIn': EmbeddedLogIn.validate, 'SystemImploded': SystemImploded.validate})

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['EmbeddedEvent']:
        return validation.validate_from_string(string, EmbeddedEvent.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        raise NotImplementedError('`to_json` is not implemented for base class `EmbeddedEvent`')

    def encode(self) -> str:
        raise NotImplementedError('`encode` is not implemented for base class `EmbeddedEvent`')

@dataclass
class EmbeddedLogIn(EmbeddedEvent):
    username: str
    password: str

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['EmbeddedLogIn']:
        return validation.validate_with_type_tag_and_validator(value, 'type', 'EmbeddedLogIn', LogInData.validate, EmbeddedLogIn)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['EmbeddedLogIn']:
        return validation.validate_from_string(string, EmbeddedLogIn.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'type': 'EmbeddedLogIn', **LogInData.to_json(self)}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass
class SystemImploded(EmbeddedEvent):
    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['SystemImploded']:
        return validation.validate_with_type_tag_and_validator(value, 'type', 'SystemImploded', validation.validate_unknown, SystemImploded)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['SystemImploded']:
        return validation.validate_from_string(string, SystemImploded.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'type': 'SystemImploded'}

    def encode(self) -> str:
        return json.dumps(self.to_json())