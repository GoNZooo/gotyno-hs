package org.gotynoOutput

import com.fasterxml.jackson.annotation.*
import com.fasterxml.jackson.module.kotlin.*
import java.math.BigInteger

class Github {
data class UserData(
    val login: String,
    val id: Int,
    val avatar_url: String,
    val url: String,
    val html_url: String,
    val followers_url: String,
    val gists_url: String,
    val repos_url: String,
    val site_admin: Boolean,
    val bio: String,
    val public_repos: Int,
    val followers: Int,
    val following: Int,
    val created_at: String,
    val updated_at: String,
    val location: String?,
    val blog: String?
)

data class OwnerData(
    val id: Int,
    val login: String,
    val url: String,
    val html_url: String,
    val followers_url: String,
    val gists_url: String,
    val repos_url: String,
    val site_admin: Boolean
)

data class OrganizationData(
    val login: String,
    val id: Int,
    val avatar_url: String,
    val members_url: String?,
    val repos_url: String,
    val description: String?
)

@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.PROPERTY,
    property = "type"
)
sealed class Owner {
    @JsonTypeName("User")
    data class User(@JsonValue(true) val data: OwnerData) : Owner()

    @JsonTypeName("Organization")
    data class Organization(@JsonValue(true) val data: OrganizationData) : Owner()
}

data class Repository(
    val id: Int,
    val name: String,
    val full_name: String,
    val private: Boolean,
    val fork: Boolean,
    val created_at: String,
    val updated_at: String,
    val description: String?,
    val owner: Owner,
    val url: String,
    val html_url: String,
    val language: String?
)

data class Pusher(
    val name: String,
    val email: String
)

data class Author(
    val name: String,
    val email: String,
    val username: String
)

data class Label(
    val id: Int,
    val url: String,
    val name: String,
    val color: String,
    val default: Boolean,
    val description: String
)

data class Issue(
    val id: Int,
    val url: String,
    val html_url: String,
    val repository_url: String,
    val number: Int,
    val title: String,
    val user: UserData,
    val labels: Array<Label>,
    val state: String,
    val locked: Boolean,
    val assignee: UserData?,
    val assignees: Array<UserData>,
    val comments: Int,
    val created_at: String,
    val updated_at: String,
    val closed_at: String?,
    val author_association: String,
    val body: String
)

data class Commit(
    val id: String,
    val tree_id: String,
    val distinct: Boolean,
    val message: String,
    val timestamp: String,
    val url: String,
    val author: Author,
    val committer: Author,
    val added: Array<String>,
    val removed: Array<String>,
    val modified: Array<String>
)

data class PushData(
    val repository: Repository,
    val ref: String,
    val before: String,
    val after: String,
    val pusher: Pusher,
    val organization: OrganizationData,
    val sender: UserData,
    val created: Boolean,
    val deleted: Boolean,
    val forced: Boolean,
    val compare: String,
    val commits: Array<Commit>,
    val head_commit: Commit
)

@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.PROPERTY,
    property = "type"
)
sealed class WebhookEvent {
    @JsonTypeName("push")
    data class Push(val data: PushData) : WebhookEvent()
}

data class RepositorySearchData(
    val total_count: Int,
    val incomplete_results: Boolean,
    val items: Array<Repository>
)
}