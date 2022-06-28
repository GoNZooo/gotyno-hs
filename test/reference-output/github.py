import enum
import json
import typing
from dataclasses import dataclass
from gotyno_validation import encoding, validation

@dataclass(frozen=True)
class UserData:
    login: str
    id: int
    avatar_url: str
    url: str
    html_url: str
    followers_url: str
    gists_url: str
    repos_url: str
    site_admin: bool
    bio: str
    public_repos: int
    followers: int
    following: int
    created_at: str
    updated_at: str
    location: typing.Optional[str]
    blog: typing.Optional[str]

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['UserData']:
        return validation.validate_interface(value, {'login': validation.validate_string, 'id': validation.validate_int, 'avatar_url': validation.validate_string, 'url': validation.validate_string, 'html_url': validation.validate_string, 'followers_url': validation.validate_string, 'gists_url': validation.validate_string, 'repos_url': validation.validate_string, 'site_admin': validation.validate_bool, 'bio': validation.validate_string, 'public_repos': validation.validate_int, 'followers': validation.validate_int, 'following': validation.validate_int, 'created_at': validation.validate_string, 'updated_at': validation.validate_string, 'location': validation.validate_optional(validation.validate_string), 'blog': validation.validate_optional(validation.validate_string)}, UserData)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['UserData']:
        return validation.validate_from_string(string, UserData.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'login': self.login, 'id': self.id, 'avatar_url': self.avatar_url, 'url': self.url, 'html_url': self.html_url, 'followers_url': self.followers_url, 'gists_url': self.gists_url, 'repos_url': self.repos_url, 'site_admin': self.site_admin, 'bio': self.bio, 'public_repos': self.public_repos, 'followers': self.followers, 'following': self.following, 'created_at': self.created_at, 'updated_at': self.updated_at, 'location': encoding.optional_to_json(encoding.basic_to_json)(self.location), 'blog': encoding.optional_to_json(encoding.basic_to_json)(self.blog)}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass(frozen=True)
class OwnerData:
    id: int
    login: str
    url: str
    html_url: str
    followers_url: str
    gists_url: str
    repos_url: str
    site_admin: bool

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['OwnerData']:
        return validation.validate_interface(value, {'id': validation.validate_int, 'login': validation.validate_string, 'url': validation.validate_string, 'html_url': validation.validate_string, 'followers_url': validation.validate_string, 'gists_url': validation.validate_string, 'repos_url': validation.validate_string, 'site_admin': validation.validate_bool}, OwnerData)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['OwnerData']:
        return validation.validate_from_string(string, OwnerData.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'id': self.id, 'login': self.login, 'url': self.url, 'html_url': self.html_url, 'followers_url': self.followers_url, 'gists_url': self.gists_url, 'repos_url': self.repos_url, 'site_admin': self.site_admin}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass(frozen=True)
class OrganizationData:
    login: str
    id: int
    avatar_url: str
    members_url: typing.Optional[str]
    repos_url: str
    description: typing.Optional[str]

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['OrganizationData']:
        return validation.validate_interface(value, {'login': validation.validate_string, 'id': validation.validate_int, 'avatar_url': validation.validate_string, 'members_url': validation.validate_optional(validation.validate_string), 'repos_url': validation.validate_string, 'description': validation.validate_optional(validation.validate_string)}, OrganizationData)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['OrganizationData']:
        return validation.validate_from_string(string, OrganizationData.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'login': self.login, 'id': self.id, 'avatar_url': self.avatar_url, 'members_url': encoding.optional_to_json(encoding.basic_to_json)(self.members_url), 'repos_url': self.repos_url, 'description': encoding.optional_to_json(encoding.basic_to_json)(self.description)}

    def encode(self) -> str:
        return json.dumps(self.to_json())

class Owner:
    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['Owner']:
        return validation.validate_with_type_tags(value, 'type', {'User': User.validate, 'Organization': Organization.validate})

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['Owner']:
        return validation.validate_from_string(string, Owner.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        raise NotImplementedError('`to_json` is not implemented for base class `Owner`')

    def encode(self) -> str:
        raise NotImplementedError('`encode` is not implemented for base class `Owner`')

@dataclass
class User(Owner):
    id: int
    login: str
    url: str
    html_url: str
    followers_url: str
    gists_url: str
    repos_url: str
    site_admin: bool

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['User']:
        return validation.validate_with_type_tag_and_validator(value, 'type', 'User', OwnerData.validate, User)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['User']:
        return validation.validate_from_string(string, User.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'type': 'User', **OwnerData.to_json(self)}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass
class Organization(Owner):
    login: str
    id: int
    avatar_url: str
    members_url: typing.Optional[str]
    repos_url: str
    description: typing.Optional[str]

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['Organization']:
        return validation.validate_with_type_tag_and_validator(value, 'type', 'Organization', OrganizationData.validate, Organization)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['Organization']:
        return validation.validate_from_string(string, Organization.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'type': 'Organization', **OrganizationData.to_json(self)}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass(frozen=True)
class Repository:
    id: int
    name: str
    full_name: str
    private: bool
    fork: bool
    created_at: str
    updated_at: str
    description: typing.Optional[str]
    owner: Owner
    url: str
    html_url: str
    language: typing.Optional[str]

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['Repository']:
        return validation.validate_interface(value, {'id': validation.validate_int, 'name': validation.validate_string, 'full_name': validation.validate_string, 'private': validation.validate_bool, 'fork': validation.validate_bool, 'created_at': validation.validate_string, 'updated_at': validation.validate_string, 'description': validation.validate_optional(validation.validate_string), 'owner': Owner.validate, 'url': validation.validate_string, 'html_url': validation.validate_string, 'language': validation.validate_optional(validation.validate_string)}, Repository)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['Repository']:
        return validation.validate_from_string(string, Repository.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'id': self.id, 'name': self.name, 'full_name': self.full_name, 'private': self.private, 'fork': self.fork, 'created_at': self.created_at, 'updated_at': self.updated_at, 'description': encoding.optional_to_json(encoding.basic_to_json)(self.description), 'owner': Owner.to_json(self.owner), 'url': self.url, 'html_url': self.html_url, 'language': encoding.optional_to_json(encoding.basic_to_json)(self.language)}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass(frozen=True)
class Pusher:
    name: str
    email: str

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['Pusher']:
        return validation.validate_interface(value, {'name': validation.validate_string, 'email': validation.validate_string}, Pusher)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['Pusher']:
        return validation.validate_from_string(string, Pusher.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'name': self.name, 'email': self.email}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass(frozen=True)
class Author:
    name: str
    email: str
    username: str

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['Author']:
        return validation.validate_interface(value, {'name': validation.validate_string, 'email': validation.validate_string, 'username': validation.validate_string}, Author)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['Author']:
        return validation.validate_from_string(string, Author.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'name': self.name, 'email': self.email, 'username': self.username}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass(frozen=True)
class Label:
    id: int
    url: str
    name: str
    color: str
    default: bool
    description: str

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['Label']:
        return validation.validate_interface(value, {'id': validation.validate_int, 'url': validation.validate_string, 'name': validation.validate_string, 'color': validation.validate_string, 'default': validation.validate_bool, 'description': validation.validate_string}, Label)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['Label']:
        return validation.validate_from_string(string, Label.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'id': self.id, 'url': self.url, 'name': self.name, 'color': self.color, 'default': self.default, 'description': self.description}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass(frozen=True)
class Issue:
    id: int
    url: str
    html_url: str
    repository_url: str
    number: int
    title: str
    user: UserData
    labels: typing.List[Label]
    state: str
    locked: bool
    assignee: typing.Optional[UserData]
    assignees: typing.List[UserData]
    comments: int
    created_at: str
    updated_at: str
    closed_at: typing.Optional[str]
    author_association: str
    body: str

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['Issue']:
        return validation.validate_interface(value, {'id': validation.validate_int, 'url': validation.validate_string, 'html_url': validation.validate_string, 'repository_url': validation.validate_string, 'number': validation.validate_int, 'title': validation.validate_string, 'user': UserData.validate, 'labels': validation.validate_list(Label.validate), 'state': validation.validate_string, 'locked': validation.validate_bool, 'assignee': validation.validate_optional(UserData.validate), 'assignees': validation.validate_list(UserData.validate), 'comments': validation.validate_int, 'created_at': validation.validate_string, 'updated_at': validation.validate_string, 'closed_at': validation.validate_optional(validation.validate_string), 'author_association': validation.validate_string, 'body': validation.validate_string}, Issue)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['Issue']:
        return validation.validate_from_string(string, Issue.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'id': self.id, 'url': self.url, 'html_url': self.html_url, 'repository_url': self.repository_url, 'number': self.number, 'title': self.title, 'user': UserData.to_json(self.user), 'labels': encoding.list_to_json(Label.to_json)(self.labels), 'state': self.state, 'locked': self.locked, 'assignee': encoding.optional_to_json(UserData.to_json)(self.assignee), 'assignees': encoding.list_to_json(UserData.to_json)(self.assignees), 'comments': self.comments, 'created_at': self.created_at, 'updated_at': self.updated_at, 'closed_at': encoding.optional_to_json(encoding.basic_to_json)(self.closed_at), 'author_association': self.author_association, 'body': self.body}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass(frozen=True)
class Commit:
    id: str
    tree_id: str
    distinct: bool
    message: str
    timestamp: str
    url: str
    author: Author
    committer: Author
    added: typing.List[str]
    removed: typing.List[str]
    modified: typing.List[str]

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['Commit']:
        return validation.validate_interface(value, {'id': validation.validate_string, 'tree_id': validation.validate_string, 'distinct': validation.validate_bool, 'message': validation.validate_string, 'timestamp': validation.validate_string, 'url': validation.validate_string, 'author': Author.validate, 'committer': Author.validate, 'added': validation.validate_list(validation.validate_string), 'removed': validation.validate_list(validation.validate_string), 'modified': validation.validate_list(validation.validate_string)}, Commit)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['Commit']:
        return validation.validate_from_string(string, Commit.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'id': self.id, 'tree_id': self.tree_id, 'distinct': self.distinct, 'message': self.message, 'timestamp': self.timestamp, 'url': self.url, 'author': Author.to_json(self.author), 'committer': Author.to_json(self.committer), 'added': encoding.list_to_json(encoding.basic_to_json)(self.added), 'removed': encoding.list_to_json(encoding.basic_to_json)(self.removed), 'modified': encoding.list_to_json(encoding.basic_to_json)(self.modified)}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass(frozen=True)
class PushData:
    repository: Repository
    ref: str
    before: str
    after: str
    pusher: Pusher
    organization: OrganizationData
    sender: UserData
    created: bool
    deleted: bool
    forced: bool
    compare: str
    commits: typing.List[Commit]
    head_commit: Commit

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['PushData']:
        return validation.validate_interface(value, {'repository': Repository.validate, 'ref': validation.validate_string, 'before': validation.validate_string, 'after': validation.validate_string, 'pusher': Pusher.validate, 'organization': OrganizationData.validate, 'sender': UserData.validate, 'created': validation.validate_bool, 'deleted': validation.validate_bool, 'forced': validation.validate_bool, 'compare': validation.validate_string, 'commits': validation.validate_list(Commit.validate), 'head_commit': Commit.validate}, PushData)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['PushData']:
        return validation.validate_from_string(string, PushData.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'repository': Repository.to_json(self.repository), 'ref': self.ref, 'before': self.before, 'after': self.after, 'pusher': Pusher.to_json(self.pusher), 'organization': OrganizationData.to_json(self.organization), 'sender': UserData.to_json(self.sender), 'created': self.created, 'deleted': self.deleted, 'forced': self.forced, 'compare': self.compare, 'commits': encoding.list_to_json(Commit.to_json)(self.commits), 'head_commit': Commit.to_json(self.head_commit)}

    def encode(self) -> str:
        return json.dumps(self.to_json())

class WebhookEvent:
    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['WebhookEvent']:
        return validation.validate_with_type_tags(value, 'type', {'push': Push.validate})

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['WebhookEvent']:
        return validation.validate_from_string(string, WebhookEvent.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        raise NotImplementedError('`to_json` is not implemented for base class `WebhookEvent`')

    def encode(self) -> str:
        raise NotImplementedError('`encode` is not implemented for base class `WebhookEvent`')

@dataclass(frozen=True)
class push(WebhookEvent):
    data: PushData

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['push']:
        return validation.validate_with_type_tag(value, 'type', 'push', {'data': PushData.validate}, push)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['push']:
        return validation.validate_from_string(string, push.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'type': 'push', 'data': self.data.to_json()}

    def encode(self) -> str:
        return json.dumps(self.to_json())

@dataclass(frozen=True)
class RepositorySearchData:
    total_count: int
    incomplete_results: bool
    items: typing.List[Repository]

    @staticmethod
    def validate(value: validation.Unknown) -> validation.ValidationResult['RepositorySearchData']:
        return validation.validate_interface(value, {'total_count': validation.validate_int, 'incomplete_results': validation.validate_bool, 'items': validation.validate_list(Repository.validate)}, RepositorySearchData)

    @staticmethod
    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['RepositorySearchData']:
        return validation.validate_from_string(string, RepositorySearchData.validate)

    def to_json(self) -> typing.Dict[str, typing.Any]:
        return {'total_count': self.total_count, 'incomplete_results': self.incomplete_results, 'items': encoding.list_to_json(Repository.to_json)(self.items)}

    def encode(self) -> str:
        return json.dumps(self.to_json())