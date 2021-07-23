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
        return validation.validate_interface(value, {'type': validation.validate_literal('SomeType'), 'some_field': validation.validate_string, 'some_other_field': validation.validate_int, 'maybe_some_field': validation.validate_optional(validation.validate_string)}, SomeType)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['SomeType']:
        return validation.validate_from_string(string, SomeType.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'type': 'SomeType', 'some_field': self.some_field, 'some_other_field': self.some_other_field, 'maybe_some_field': encoding.optional_to_json(encoding.basic_to_json)(self.maybe_some_field)}

    def encode(self) -> str:
        return json.dumps(self.to_json())

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

class Event:
    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['Event']:
        return validation.validate_with_type_tags(value, 'type', {'Notification': Notification.validate, 'Launch': Launch.validate, 'AnotherEvent': AnotherEvent.validate})

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['Event']:
        return validation.validate_from_string(string, Event.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        raise NotImplementedError('`to_json` is not implemented for base class `Event`')

    def encode(self) -> str:
        raise NotImplementedError('`encode` is not implemented for base class `Event`')

@dataclass(frozen=True)
class Notification(Event):
    data: str

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['Notification']:
        return validation.validate_with_type_tag(value, 'type', 'Notification', {'data': validation.validate_string}, Notification)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['Notification']:
        return validation.validate_from_string(string, Notification.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'type': 'Notification', 'data': self.data}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass(frozen=True)
class Launch(Event):
    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['Launch']:
        return validation.validate_with_type_tag(value, 'type', 'Launch', {}, Launch)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['Launch']:
        return validation.validate_from_string(string, Launch.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'type': 'Launch'}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass(frozen=True)
class AnotherEvent(Event):
    data: SomeType

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['AnotherEvent']:
        return validation.validate_with_type_tag(value, 'type', 'AnotherEvent', {'data': SomeType.validate}, AnotherEvent)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['AnotherEvent']:
        return validation.validate_from_string(string, AnotherEvent.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'type': 'AnotherEvent', 'data': self.data.to_json()}

    def encode(self) -> str:
        return json.dumps(self.to_json())

class EventWithKind:
    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['EventWithKind']:
        return validation.validate_with_type_tags(value, 'kind', {'NotificationWithKind': NotificationWithKind.validate, 'LaunchWithKind': LaunchWithKind.validate, 'AnotherEventWithKind': AnotherEventWithKind.validate})

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['EventWithKind']:
        return validation.validate_from_string(string, EventWithKind.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        raise NotImplementedError('`to_json` is not implemented for base class `EventWithKind`')

    def encode(self) -> str:
        raise NotImplementedError('`encode` is not implemented for base class `EventWithKind`')

@dataclass(frozen=True)
class NotificationWithKind(EventWithKind):
    data: str

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['NotificationWithKind']:
        return validation.validate_with_type_tag(value, 'kind', 'NotificationWithKind', {'data': validation.validate_string}, NotificationWithKind)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['NotificationWithKind']:
        return validation.validate_from_string(string, NotificationWithKind.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'kind': 'NotificationWithKind', 'data': self.data}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass(frozen=True)
class LaunchWithKind(EventWithKind):
    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['LaunchWithKind']:
        return validation.validate_with_type_tag(value, 'kind', 'LaunchWithKind', {}, LaunchWithKind)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['LaunchWithKind']:
        return validation.validate_from_string(string, LaunchWithKind.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'kind': 'LaunchWithKind'}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass(frozen=True)
class AnotherEventWithKind(EventWithKind):
    data: SomeType

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['AnotherEventWithKind']:
        return validation.validate_with_type_tag(value, 'kind', 'AnotherEventWithKind', {'data': SomeType.validate}, AnotherEventWithKind)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['AnotherEventWithKind']:
        return validation.validate_from_string(string, AnotherEventWithKind.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'kind': 'AnotherEventWithKind', 'data': self.data.to_json()}

    def encode(self) -> str:
        return json.dumps(self.to_json())

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

    def to_json(self) -> typing.Dict[str, typing.Any]:
        raise NotImplementedError('`to_json` is not implemented for base class `Possibly`')

    def encode(self) -> str:
        raise NotImplementedError('`encode` is not implemented for base class `Possibly`')

@dataclass(frozen=True)
class NotReally(Possibly[T]):
    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['NotReally']:
        return validation.validate_with_type_tag(value, 'type', 'NotReally', {}, NotReally)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['NotReally']:
        return validation.validate_from_string(string, NotReally.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'type': 'NotReally'}

    def encode(self) -> str:
        return json.dumps(self.to_json())

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

    def to_json(self, T_to_json: encoding.ToJSON[T]) -> typing.Dict[str, typing.Any]:
        return {'type': 'Definitely', 'data': T_to_json(self.data)}

    def encode(self, T_to_json: encoding.ToJSON[T]) -> str:
        return json.dumps(self.to_json(T_to_json))

class Color(enum.Enum):
    red = 'ff0000'
    green = '00ff00'
    blue = '0000ff'

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['Color']:
        return validation.validate_enumeration_member(value, Color)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['Color']:
        return validation.validate_from_string(string, Color.validate)

    def to_json(self) -> typing.Any:
        return self.value

    def encode(self) -> str:
        return str(self.value)

@dataclass(frozen=True)
class KnownForMovieWithoutTypeTag:
    poster_path: typing.Optional[str]
    id: int
    title: typing.Optional[str]
    vote_average: float
    release_date: typing.Optional[str]
    overview: str

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['KnownForMovieWithoutTypeTag']:
        return validation.validate_interface(value, {'poster_path': validation.validate_optional(validation.validate_string), 'id': validation.validate_int, 'title': validation.validate_optional(validation.validate_string), 'vote_average': validation.validate_float, 'release_date': validation.validate_optional(validation.validate_string), 'overview': validation.validate_string}, KnownForMovieWithoutTypeTag)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['KnownForMovieWithoutTypeTag']:
        return validation.validate_from_string(string, KnownForMovieWithoutTypeTag.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'poster_path': encoding.optional_to_json(encoding.basic_to_json)(self.poster_path), 'id': self.id, 'title': encoding.optional_to_json(encoding.basic_to_json)(self.title), 'vote_average': self.vote_average, 'release_date': encoding.optional_to_json(encoding.basic_to_json)(self.release_date), 'overview': self.overview}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass(frozen=True)
class KnownForShowWithoutTypeTag:
    poster_path: typing.Optional[str]
    id: int
    vote_average: float
    overview: str
    first_air_date: typing.Optional[str]
    name: typing.Optional[str]

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['KnownForShowWithoutTypeTag']:
        return validation.validate_interface(value, {'poster_path': validation.validate_optional(validation.validate_string), 'id': validation.validate_int, 'vote_average': validation.validate_float, 'overview': validation.validate_string, 'first_air_date': validation.validate_optional(validation.validate_string), 'name': validation.validate_optional(validation.validate_string)}, KnownForShowWithoutTypeTag)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['KnownForShowWithoutTypeTag']:
        return validation.validate_from_string(string, KnownForShowWithoutTypeTag.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'poster_path': encoding.optional_to_json(encoding.basic_to_json)(self.poster_path), 'id': self.id, 'vote_average': self.vote_average, 'overview': self.overview, 'first_air_date': encoding.optional_to_json(encoding.basic_to_json)(self.first_air_date), 'name': encoding.optional_to_json(encoding.basic_to_json)(self.name)}

    def encode(self) -> str:
        return json.dumps(self.to_json())

class KnownForEmbedded:
    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['KnownForEmbedded']:
        return validation.validate_with_type_tags(value, 'media_type', {'movieStartingWithLowercase': MovieStartingWithLowercase.validate, 'tvStartingWithLowercase': TvStartingWithLowercase.validate})

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['KnownForEmbedded']:
        return validation.validate_from_string(string, KnownForEmbedded.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        raise NotImplementedError('`to_json` is not implemented for base class `KnownForEmbedded`')

    def encode(self) -> str:
        raise NotImplementedError('`encode` is not implemented for base class `KnownForEmbedded`')

@dataclass
class MovieStartingWithLowercase(KnownForEmbedded):
    poster_path: typing.Optional[str]
    id: int
    title: typing.Optional[str]
    vote_average: float
    release_date: typing.Optional[str]
    overview: str

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['MovieStartingWithLowercase']:
        return validation.validate_with_type_tag_and_validator(value, 'media_type', 'movieStartingWithLowercase', KnownForMovieWithoutTypeTag.validate, MovieStartingWithLowercase)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['MovieStartingWithLowercase']:
        return validation.validate_from_string(string, MovieStartingWithLowercase.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'media_type': 'movieStartingWithLowercase', **KnownForMovieWithoutTypeTag.to_json(self)}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass
class TvStartingWithLowercase(KnownForEmbedded):
    poster_path: typing.Optional[str]
    id: int
    vote_average: float
    overview: str
    first_air_date: typing.Optional[str]
    name: typing.Optional[str]

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['TvStartingWithLowercase']:
        return validation.validate_with_type_tag_and_validator(value, 'media_type', 'tvStartingWithLowercase', KnownForShowWithoutTypeTag.validate, TvStartingWithLowercase)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['TvStartingWithLowercase']:
        return validation.validate_from_string(string, TvStartingWithLowercase.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'media_type': 'tvStartingWithLowercase', **KnownForShowWithoutTypeTag.to_json(self)}

    def encode(self) -> str:
        return json.dumps(self.to_json())

class KnownForEmbeddedWithUpperCase:
    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['KnownForEmbeddedWithUpperCase']:
        return validation.validate_with_type_tags(value, 'media_type', {'Movie': Movie.validate, 'Tv': Tv.validate})

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['KnownForEmbeddedWithUpperCase']:
        return validation.validate_from_string(string, KnownForEmbeddedWithUpperCase.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        raise NotImplementedError('`to_json` is not implemented for base class `KnownForEmbeddedWithUpperCase`')

    def encode(self) -> str:
        raise NotImplementedError('`encode` is not implemented for base class `KnownForEmbeddedWithUpperCase`')

@dataclass
class Movie(KnownForEmbeddedWithUpperCase):
    poster_path: typing.Optional[str]
    id: int
    title: typing.Optional[str]
    vote_average: float
    release_date: typing.Optional[str]
    overview: str

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['Movie']:
        return validation.validate_with_type_tag_and_validator(value, 'media_type', 'Movie', KnownForMovieWithoutTypeTag.validate, Movie)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['Movie']:
        return validation.validate_from_string(string, Movie.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'media_type': 'Movie', **KnownForMovieWithoutTypeTag.to_json(self)}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass
class Tv(KnownForEmbeddedWithUpperCase):
    poster_path: typing.Optional[str]
    id: int
    vote_average: float
    overview: str
    first_air_date: typing.Optional[str]
    name: typing.Optional[str]

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['Tv']:
        return validation.validate_with_type_tag_and_validator(value, 'media_type', 'Tv', KnownForShowWithoutTypeTag.validate, Tv)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['Tv']:
        return validation.validate_from_string(string, Tv.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'media_type': 'Tv', **KnownForShowWithoutTypeTag.to_json(self)}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass(frozen=True)
class KnownForMovie:
    media_type: typing.Literal['movie']
    poster_path: typing.Optional[str]
    id: int
    title: typing.Optional[str]
    vote_average: float
    release_date: typing.Optional[str]
    overview: str

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['KnownForMovie']:
        return validation.validate_interface(value, {'media_type': validation.validate_literal('movie'), 'poster_path': validation.validate_optional(validation.validate_string), 'id': validation.validate_int, 'title': validation.validate_optional(validation.validate_string), 'vote_average': validation.validate_float, 'release_date': validation.validate_optional(validation.validate_string), 'overview': validation.validate_string}, KnownForMovie)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['KnownForMovie']:
        return validation.validate_from_string(string, KnownForMovie.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'media_type': 'movie', 'poster_path': encoding.optional_to_json(encoding.basic_to_json)(self.poster_path), 'id': self.id, 'title': encoding.optional_to_json(encoding.basic_to_json)(self.title), 'vote_average': self.vote_average, 'release_date': encoding.optional_to_json(encoding.basic_to_json)(self.release_date), 'overview': self.overview}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass(frozen=True)
class KnownForShow:
    media_type: typing.Literal['tv']
    poster_path: typing.Optional[str]
    id: int
    vote_average: float
    overview: str
    first_air_date: typing.Optional[str]
    name: typing.Optional[str]

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['KnownForShow']:
        return validation.validate_interface(value, {'media_type': validation.validate_literal('tv'), 'poster_path': validation.validate_optional(validation.validate_string), 'id': validation.validate_int, 'vote_average': validation.validate_float, 'overview': validation.validate_string, 'first_air_date': validation.validate_optional(validation.validate_string), 'name': validation.validate_optional(validation.validate_string)}, KnownForShow)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['KnownForShow']:
        return validation.validate_from_string(string, KnownForShow.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'media_type': 'tv', 'poster_path': encoding.optional_to_json(encoding.basic_to_json)(self.poster_path), 'id': self.id, 'vote_average': self.vote_average, 'overview': self.overview, 'first_air_date': encoding.optional_to_json(encoding.basic_to_json)(self.first_air_date), 'name': encoding.optional_to_json(encoding.basic_to_json)(self.name)}

    def encode(self) -> str:
        return json.dumps(self.to_json())

KnownFor = typing.Union[KnownForShow, KnownForMovie, str, float]
class KnownForInterface:
    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['KnownFor']:
        return validation.validate_one_of(value, [KnownForShow.validate, KnownForMovie.validate, validation.validate_string, validation.validate_float])

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['KnownFor']:
        return validation.validate_from_string(string, KnownForInterface.validate)

    @staticmethod
    def to_json(value) -> typing.Any:
        return encoding.one_of_to_json(value, {KnownForShow: KnownForShow.to_json, KnownForMovie: KnownForMovie.to_json, str: encoding.basic_to_json, float: encoding.basic_to_json})

    @staticmethod
    def encode(value) -> str:
        return json.dumps(value.to_json())