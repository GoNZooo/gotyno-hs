import enum
import json
import typing
from dataclasses import dataclass
from gotyno_validation import encoding, validation

from . import basic
from . import hasGeneric

@dataclass(frozen=True)
class UsingGenerics:
    field1: basic.Maybe[str]
    field2: basic.Either[str, int]

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['UsingGenerics']:
        return validation.validate_interface(value, {'field1': basic.Maybe.validate(validation.validate_string), 'field2': basic.Either.validate(validation.validate_string, validation.validate_int)}, UsingGenerics)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['UsingGenerics']:
        return validation.validate_from_string(string, UsingGenerics.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'field1': self.field1.to_json(encoding.basic_to_json), 'field2': self.field2.to_json(encoding.basic_to_json, encoding.basic_to_json)}

    def encode(self) -> str:
        return json.dumps(self.to_json())

T = typing.TypeVar('T')
@dataclass(frozen=True)
class UsingOwnGenerics(typing.Generic[T]):
    field1: basic.Maybe[T]

    @staticmethod
    def validate(validate_T: validation.Validator[T]) -> validation.Validator['UsingOwnGenerics[T]']:
        def validate_UsingOwnGenericsT(value: validation.Unknown) -> validation.ValidationResult['UsingOwnGenerics[T]']:
            return validation.validate_interface(value, {'field1': basic.Maybe.validate(validate_T)}, UsingOwnGenerics)
        return validate_UsingOwnGenericsT

    @staticmethod
    def decode(string: typing.Union[str, bytes], validate_T: validation.Validator[T]) -> validation.ValidationResult['UsingOwnGenerics[T]']:
        return validation.validate_from_string(string, UsingOwnGenerics.validate(validate_T))

    def to_json(self, T_to_json: encoding.ToJSON[T]) -> typing.Dict[str, typing.Any]:
        return {'field1': self.field1.to_json(T_to_json)}

    def encode(self, T_to_json: encoding.ToJSON[T]) -> str:
        return json.dumps(self.to_json(T_to_json))

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