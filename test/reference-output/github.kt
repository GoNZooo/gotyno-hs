package org.gotynoOutput

import com.fasterxml.jackson.annotation.*
import com.fasterxml.jackson.module.kotlin.*
import com.fasterxml.jackson.databind.annotation.*
import com.fasterxml.jackson.databind.*
import com.fasterxml.jackson.core.*
import com.fasterxml.jackson.databind.deser.std.*
import java.text.ParseException
import java.math.BigInteger

class Github {
data class UserData(
    @JsonProperty("login")
    val login: String,
    @JsonProperty("id")
    val id: Int,
    @JsonProperty("avatar_url")
    val avatar_url: String,
    @JsonProperty("url")
    val url: String,
    @JsonProperty("html_url")
    val html_url: String,
    @JsonProperty("followers_url")
    val followers_url: String,
    @JsonProperty("gists_url")
    val gists_url: String,
    @JsonProperty("repos_url")
    val repos_url: String,
    @JsonProperty("site_admin")
    val site_admin: Boolean,
    @JsonProperty("bio")
    val bio: String,
    @JsonProperty("public_repos")
    val public_repos: Int,
    @JsonProperty("followers")
    val followers: Int,
    @JsonProperty("following")
    val following: Int,
    @JsonProperty("created_at")
    val created_at: String,
    @JsonProperty("updated_at")
    val updated_at: String,
    @JsonProperty("location")
    val location: String?,
    @JsonProperty("blog")
    val blog: String?
)

data class OwnerData(
    @JsonProperty("id")
    val id: Int,
    @JsonProperty("login")
    val login: String,
    @JsonProperty("url")
    val url: String,
    @JsonProperty("html_url")
    val html_url: String,
    @JsonProperty("followers_url")
    val followers_url: String,
    @JsonProperty("gists_url")
    val gists_url: String,
    @JsonProperty("repos_url")
    val repos_url: String,
    @JsonProperty("site_admin")
    val site_admin: Boolean
)

data class OrganizationData(
    @JsonProperty("login")
    val login: String,
    @JsonProperty("id")
    val id: Int,
    @JsonProperty("avatar_url")
    val avatar_url: String,
    @JsonProperty("members_url")
    val members_url: String?,
    @JsonProperty("repos_url")
    val repos_url: String,
    @JsonProperty("description")
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
    @JsonProperty("id")
    val id: Int,
    @JsonProperty("name")
    val name: String,
    @JsonProperty("full_name")
    val full_name: String,
    @JsonProperty("private")
    val private: Boolean,
    @JsonProperty("fork")
    val fork: Boolean,
    @JsonProperty("created_at")
    val created_at: String,
    @JsonProperty("updated_at")
    val updated_at: String,
    @JsonProperty("description")
    val description: String?,
    @JsonProperty("owner")
    val owner: Owner,
    @JsonProperty("url")
    val url: String,
    @JsonProperty("html_url")
    val html_url: String,
    @JsonProperty("language")
    val language: String?
)

data class Pusher(
    @JsonProperty("name")
    val name: String,
    @JsonProperty("email")
    val email: String
)

data class Author(
    @JsonProperty("name")
    val name: String,
    @JsonProperty("email")
    val email: String,
    @JsonProperty("username")
    val username: String
)

data class Label(
    @JsonProperty("id")
    val id: Int,
    @JsonProperty("url")
    val url: String,
    @JsonProperty("name")
    val name: String,
    @JsonProperty("color")
    val color: String,
    @JsonProperty("default")
    val default: Boolean,
    @JsonProperty("description")
    val description: String
)

data class Issue(
    @JsonProperty("id")
    val id: Int,
    @JsonProperty("url")
    val url: String,
    @JsonProperty("html_url")
    val html_url: String,
    @JsonProperty("repository_url")
    val repository_url: String,
    @JsonProperty("number")
    val number: Int,
    @JsonProperty("title")
    val title: String,
    @JsonProperty("user")
    val user: UserData,
    @JsonProperty("labels")
    val labels: ArrayList<Label>,
    @JsonProperty("state")
    val state: String,
    @JsonProperty("locked")
    val locked: Boolean,
    @JsonProperty("assignee")
    val assignee: UserData?,
    @JsonProperty("assignees")
    val assignees: ArrayList<UserData>,
    @JsonProperty("comments")
    val comments: Int,
    @JsonProperty("created_at")
    val created_at: String,
    @JsonProperty("updated_at")
    val updated_at: String,
    @JsonProperty("closed_at")
    val closed_at: String?,
    @JsonProperty("author_association")
    val author_association: String,
    @JsonProperty("body")
    val body: String
)

data class Commit(
    @JsonProperty("id")
    val id: String,
    @JsonProperty("tree_id")
    val tree_id: String,
    @JsonProperty("distinct")
    val distinct: Boolean,
    @JsonProperty("message")
    val message: String,
    @JsonProperty("timestamp")
    val timestamp: String,
    @JsonProperty("url")
    val url: String,
    @JsonProperty("author")
    val author: Author,
    @JsonProperty("committer")
    val committer: Author,
    @JsonProperty("added")
    val added: ArrayList<String>,
    @JsonProperty("removed")
    val removed: ArrayList<String>,
    @JsonProperty("modified")
    val modified: ArrayList<String>
)

data class PushData(
    @JsonProperty("repository")
    val repository: Repository,
    @JsonProperty("ref")
    val ref: String,
    @JsonProperty("before")
    val before: String,
    @JsonProperty("after")
    val after: String,
    @JsonProperty("pusher")
    val pusher: Pusher,
    @JsonProperty("organization")
    val organization: OrganizationData,
    @JsonProperty("sender")
    val sender: UserData,
    @JsonProperty("created")
    val created: Boolean,
    @JsonProperty("deleted")
    val deleted: Boolean,
    @JsonProperty("forced")
    val forced: Boolean,
    @JsonProperty("compare")
    val compare: String,
    @JsonProperty("commits")
    val commits: ArrayList<Commit>,
    @JsonProperty("head_commit")
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
    @JsonProperty("total_count")
    val total_count: Int,
    @JsonProperty("incomplete_results")
    val incomplete_results: Boolean,
    @JsonProperty("items")
    val items: ArrayList<Repository>
)
}