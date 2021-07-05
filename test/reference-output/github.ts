import * as svt from "simple-validation-tools";

export type UserData = {
    login: string;
    id: number;
    avatar_url: string;
    url: string;
    html_url: string;
    followers_url: string;
    gists_url: string;
    repos_url: string;
    site_admin: boolean;
    bio: string;
    public_repos: number;
    followers: number;
    following: number;
    created_at: string;
    updated_at: string;
    location: string | null | undefined;
    blog: string | null | undefined;
};

export function isUserData(value: unknown): value is UserData {
    return svt.isInterface<UserData>(value, {login: svt.isString, id: svt.isNumber, avatar_url: svt.isString, url: svt.isString, html_url: svt.isString, followers_url: svt.isString, gists_url: svt.isString, repos_url: svt.isString, site_admin: svt.isBoolean, bio: svt.isString, public_repos: svt.isNumber, followers: svt.isNumber, following: svt.isNumber, created_at: svt.isString, updated_at: svt.isString, location: svt.optional(svt.isString), blog: svt.optional(svt.isString)});
}

export function validateUserData(value: unknown): svt.ValidationResult<UserData> {
    return svt.validate<UserData>(value, {login: svt.validateString, id: svt.validateNumber, avatar_url: svt.validateString, url: svt.validateString, html_url: svt.validateString, followers_url: svt.validateString, gists_url: svt.validateString, repos_url: svt.validateString, site_admin: svt.validateBoolean, bio: svt.validateString, public_repos: svt.validateNumber, followers: svt.validateNumber, following: svt.validateNumber, created_at: svt.validateString, updated_at: svt.validateString, location: svt.validateOptional(svt.validateString), blog: svt.validateOptional(svt.validateString)});
}

export type OwnerData = {
    id: number;
    login: string;
    url: string;
    html_url: string;
    followers_url: string;
    gists_url: string;
    repos_url: string;
    site_admin: boolean;
};

export function isOwnerData(value: unknown): value is OwnerData {
    return svt.isInterface<OwnerData>(value, {id: svt.isNumber, login: svt.isString, url: svt.isString, html_url: svt.isString, followers_url: svt.isString, gists_url: svt.isString, repos_url: svt.isString, site_admin: svt.isBoolean});
}

export function validateOwnerData(value: unknown): svt.ValidationResult<OwnerData> {
    return svt.validate<OwnerData>(value, {id: svt.validateNumber, login: svt.validateString, url: svt.validateString, html_url: svt.validateString, followers_url: svt.validateString, gists_url: svt.validateString, repos_url: svt.validateString, site_admin: svt.validateBoolean});
}

export type OrganizationData = {
    login: string;
    id: number;
    avatar_url: string;
    members_url: string | null | undefined;
    repos_url: string;
    description: string | null | undefined;
};

export function isOrganizationData(value: unknown): value is OrganizationData {
    return svt.isInterface<OrganizationData>(value, {login: svt.isString, id: svt.isNumber, avatar_url: svt.isString, members_url: svt.optional(svt.isString), repos_url: svt.isString, description: svt.optional(svt.isString)});
}

export function validateOrganizationData(value: unknown): svt.ValidationResult<OrganizationData> {
    return svt.validate<OrganizationData>(value, {login: svt.validateString, id: svt.validateNumber, avatar_url: svt.validateString, members_url: svt.validateOptional(svt.validateString), repos_url: svt.validateString, description: svt.validateOptional(svt.validateString)});
}

export type Owner = User | Organization;

export enum OwnerTag {
    User = "User",
    Organization = "Organization",
}

export type User = {
    type: OwnerTag.User;
    id: number;
    login: string;
    url: string;
    html_url: string;
    followers_url: string;
    gists_url: string;
    repos_url: string;
    site_admin: boolean;
};

export type Organization = {
    type: OwnerTag.Organization;
    login: string;
    id: number;
    avatar_url: string;
    members_url: string | null | undefined;
    repos_url: string;
    description: string | null | undefined;
};

export function User(data: OwnerData): User {
    return {type: OwnerTag.User, ...data};
}

export function Organization(data: OrganizationData): Organization {
    return {type: OwnerTag.Organization, ...data};
}

export function isOwner(value: unknown): value is Owner {
    return [isUser, isOrganization].some((typePredicate) => typePredicate(value));
}

export function isUser(value: unknown): value is User {
    return svt.isInterface<User>(value, {type: OwnerTag.User, id: svt.isNumber, login: svt.isString, url: svt.isString, html_url: svt.isString, followers_url: svt.isString, gists_url: svt.isString, repos_url: svt.isString, site_admin: svt.isBoolean});
}

export function isOrganization(value: unknown): value is Organization {
    return svt.isInterface<Organization>(value, {type: OwnerTag.Organization, login: svt.isString, id: svt.isNumber, avatar_url: svt.isString, members_url: svt.optional(svt.isString), repos_url: svt.isString, description: svt.optional(svt.isString)});
}

export function validateOwner(value: unknown): svt.ValidationResult<Owner> {
    return svt.validateWithTypeTag<Owner>(value, {[OwnerTag.User]: validateUser, [OwnerTag.Organization]: validateOrganization}, "type");
}

export function validateUser(value: unknown): svt.ValidationResult<User> {
    return svt.validate<User>(value, {type: OwnerTag.User, id: svt.validateNumber, login: svt.validateString, url: svt.validateString, html_url: svt.validateString, followers_url: svt.validateString, gists_url: svt.validateString, repos_url: svt.validateString, site_admin: svt.validateBoolean});
}

export function validateOrganization(value: unknown): svt.ValidationResult<Organization> {
    return svt.validate<Organization>(value, {type: OwnerTag.Organization, login: svt.validateString, id: svt.validateNumber, avatar_url: svt.validateString, members_url: svt.validateOptional(svt.validateString), repos_url: svt.validateString, description: svt.validateOptional(svt.validateString)});
}

export type Repository = {
    id: number;
    name: string;
    full_name: string;
    private: boolean;
    fork: boolean;
    created_at: string;
    updated_at: string;
    description: string | null | undefined;
    owner: Owner;
    url: string;
    html_url: string;
    language: string | null | undefined;
};

export function isRepository(value: unknown): value is Repository {
    return svt.isInterface<Repository>(value, {id: svt.isNumber, name: svt.isString, full_name: svt.isString, private: svt.isBoolean, fork: svt.isBoolean, created_at: svt.isString, updated_at: svt.isString, description: svt.optional(svt.isString), owner: isOwner, url: svt.isString, html_url: svt.isString, language: svt.optional(svt.isString)});
}

export function validateRepository(value: unknown): svt.ValidationResult<Repository> {
    return svt.validate<Repository>(value, {id: svt.validateNumber, name: svt.validateString, full_name: svt.validateString, private: svt.validateBoolean, fork: svt.validateBoolean, created_at: svt.validateString, updated_at: svt.validateString, description: svt.validateOptional(svt.validateString), owner: validateOwner, url: svt.validateString, html_url: svt.validateString, language: svt.validateOptional(svt.validateString)});
}

export type Pusher = {
    name: string;
    email: string;
};

export function isPusher(value: unknown): value is Pusher {
    return svt.isInterface<Pusher>(value, {name: svt.isString, email: svt.isString});
}

export function validatePusher(value: unknown): svt.ValidationResult<Pusher> {
    return svt.validate<Pusher>(value, {name: svt.validateString, email: svt.validateString});
}

export type Author = {
    name: string;
    email: string;
    username: string;
};

export function isAuthor(value: unknown): value is Author {
    return svt.isInterface<Author>(value, {name: svt.isString, email: svt.isString, username: svt.isString});
}

export function validateAuthor(value: unknown): svt.ValidationResult<Author> {
    return svt.validate<Author>(value, {name: svt.validateString, email: svt.validateString, username: svt.validateString});
}

export type Label = {
    id: number;
    url: string;
    name: string;
    color: string;
    default: boolean;
    description: string;
};

export function isLabel(value: unknown): value is Label {
    return svt.isInterface<Label>(value, {id: svt.isNumber, url: svt.isString, name: svt.isString, color: svt.isString, default: svt.isBoolean, description: svt.isString});
}

export function validateLabel(value: unknown): svt.ValidationResult<Label> {
    return svt.validate<Label>(value, {id: svt.validateNumber, url: svt.validateString, name: svt.validateString, color: svt.validateString, default: svt.validateBoolean, description: svt.validateString});
}

export type Issue = {
    id: number;
    url: string;
    html_url: string;
    repository_url: string;
    number: number;
    title: string;
    user: UserData;
    labels: Label[];
    state: string;
    locked: boolean;
    assignee: UserData | null | undefined;
    assignees: UserData[];
    comments: number;
    created_at: string;
    updated_at: string;
    closed_at: string | null | undefined;
    author_association: string;
    body: string;
};

export function isIssue(value: unknown): value is Issue {
    return svt.isInterface<Issue>(value, {id: svt.isNumber, url: svt.isString, html_url: svt.isString, repository_url: svt.isString, number: svt.isNumber, title: svt.isString, user: isUserData, labels: svt.arrayOf(isLabel), state: svt.isString, locked: svt.isBoolean, assignee: svt.optional(isUserData), assignees: svt.arrayOf(isUserData), comments: svt.isNumber, created_at: svt.isString, updated_at: svt.isString, closed_at: svt.optional(svt.isString), author_association: svt.isString, body: svt.isString});
}

export function validateIssue(value: unknown): svt.ValidationResult<Issue> {
    return svt.validate<Issue>(value, {id: svt.validateNumber, url: svt.validateString, html_url: svt.validateString, repository_url: svt.validateString, number: svt.validateNumber, title: svt.validateString, user: validateUserData, labels: svt.validateArray(validateLabel), state: svt.validateString, locked: svt.validateBoolean, assignee: svt.validateOptional(validateUserData), assignees: svt.validateArray(validateUserData), comments: svt.validateNumber, created_at: svt.validateString, updated_at: svt.validateString, closed_at: svt.validateOptional(svt.validateString), author_association: svt.validateString, body: svt.validateString});
}

export type Commit = {
    id: string;
    tree_id: string;
    distinct: boolean;
    message: string;
    timestamp: string;
    url: string;
    author: Author;
    committer: Author;
    added: string[];
    removed: string[];
    modified: string[];
};

export function isCommit(value: unknown): value is Commit {
    return svt.isInterface<Commit>(value, {id: svt.isString, tree_id: svt.isString, distinct: svt.isBoolean, message: svt.isString, timestamp: svt.isString, url: svt.isString, author: isAuthor, committer: isAuthor, added: svt.arrayOf(svt.isString), removed: svt.arrayOf(svt.isString), modified: svt.arrayOf(svt.isString)});
}

export function validateCommit(value: unknown): svt.ValidationResult<Commit> {
    return svt.validate<Commit>(value, {id: svt.validateString, tree_id: svt.validateString, distinct: svt.validateBoolean, message: svt.validateString, timestamp: svt.validateString, url: svt.validateString, author: validateAuthor, committer: validateAuthor, added: svt.validateArray(svt.validateString), removed: svt.validateArray(svt.validateString), modified: svt.validateArray(svt.validateString)});
}

export type PushData = {
    repository: Repository;
    ref: string;
    before: string;
    after: string;
    pusher: Pusher;
    organization: OrganizationData;
    sender: UserData;
    created: boolean;
    deleted: boolean;
    forced: boolean;
    compare: string;
    commits: Commit[];
    head_commit: Commit;
};

export function isPushData(value: unknown): value is PushData {
    return svt.isInterface<PushData>(value, {repository: isRepository, ref: svt.isString, before: svt.isString, after: svt.isString, pusher: isPusher, organization: isOrganizationData, sender: isUserData, created: svt.isBoolean, deleted: svt.isBoolean, forced: svt.isBoolean, compare: svt.isString, commits: svt.arrayOf(isCommit), head_commit: isCommit});
}

export function validatePushData(value: unknown): svt.ValidationResult<PushData> {
    return svt.validate<PushData>(value, {repository: validateRepository, ref: svt.validateString, before: svt.validateString, after: svt.validateString, pusher: validatePusher, organization: validateOrganizationData, sender: validateUserData, created: svt.validateBoolean, deleted: svt.validateBoolean, forced: svt.validateBoolean, compare: svt.validateString, commits: svt.validateArray(validateCommit), head_commit: validateCommit});
}

export type WebhookEvent = push;

export enum WebhookEventTag {
    push = "push",
}

export type push = {
    type: WebhookEventTag.push;
    data: PushData;
};

export function push(data: PushData): push {
    return {type: WebhookEventTag.push, data};
}

export function isWebhookEvent(value: unknown): value is WebhookEvent {
    return [isPush].some((typePredicate) => typePredicate(value));
}

export function isPush(value: unknown): value is push {
    return svt.isInterface<push>(value, {type: WebhookEventTag.push, data: isPushData});
}

export function validateWebhookEvent(value: unknown): svt.ValidationResult<WebhookEvent> {
    return svt.validateWithTypeTag<WebhookEvent>(value, {[WebhookEventTag.push]: validatePush}, "type");
}

export function validatePush(value: unknown): svt.ValidationResult<push> {
    return svt.validate<push>(value, {type: WebhookEventTag.push, data: validatePushData});
}

export type RepositorySearchData = {
    total_count: number;
    incomplete_results: boolean;
    items: Repository[];
};

export function isRepositorySearchData(value: unknown): value is RepositorySearchData {
    return svt.isInterface<RepositorySearchData>(value, {total_count: svt.isNumber, incomplete_results: svt.isBoolean, items: svt.arrayOf(isRepository)});
}

export function validateRepositorySearchData(value: unknown): svt.ValidationResult<RepositorySearchData> {
    return svt.validate<RepositorySearchData>(value, {total_count: svt.validateNumber, incomplete_results: svt.validateBoolean, items: svt.validateArray(validateRepository)});
}