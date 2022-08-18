package org.gotynoOutput

import com.fasterxml.jackson.annotation.*
import com.fasterxml.jackson.module.kotlin.*
import com.fasterxml.jackson.databind.annotation.*
import com.fasterxml.jackson.databind.*
import com.fasterxml.jackson.core.*
import com.fasterxml.jackson.databind.deser.std.*
import java.text.ParseException
import java.math.BigInteger
import kotlinx.serialization.Serializable

class Github {
@Serializable
data class UserData(
    @get:JsonProperty("login")
    val login: String,
    @get:JsonProperty("id")
    val id: Int,
    @get:JsonProperty("avatar_url")
    val avatar_url: String,
    @get:JsonProperty("url")
    val url: String,
    @get:JsonProperty("html_url")
    val html_url: String,
    @get:JsonProperty("followers_url")
    val followers_url: String,
    @get:JsonProperty("gists_url")
    val gists_url: String,
    @get:JsonProperty("repos_url")
    val repos_url: String,
    @get:JsonProperty("site_admin")
    val site_admin: Boolean,
    @get:JsonProperty("bio")
    val bio: String,
    @get:JsonProperty("public_repos")
    val public_repos: Int,
    @get:JsonProperty("followers")
    val followers: Int,
    @get:JsonProperty("following")
    val following: Int,
    @get:JsonProperty("created_at")
    val created_at: String,
    @get:JsonProperty("updated_at")
    val updated_at: String,
    @get:JsonProperty("location")
    val location: String?,
    @get:JsonProperty("blog")
    val blog: String?
) : java.io.Serializable

@Serializable
data class OwnerData(
    @get:JsonProperty("id")
    val id: Int,
    @get:JsonProperty("login")
    val login: String,
    @get:JsonProperty("url")
    val url: String,
    @get:JsonProperty("html_url")
    val html_url: String,
    @get:JsonProperty("followers_url")
    val followers_url: String,
    @get:JsonProperty("gists_url")
    val gists_url: String,
    @get:JsonProperty("repos_url")
    val repos_url: String,
    @get:JsonProperty("site_admin")
    val site_admin: Boolean
) : java.io.Serializable

@Serializable
data class OrganizationData(
    @get:JsonProperty("login")
    val login: String,
    @get:JsonProperty("id")
    val id: Int,
    @get:JsonProperty("avatar_url")
    val avatar_url: String,
    @get:JsonProperty("members_url")
    val members_url: String?,
    @get:JsonProperty("repos_url")
    val repos_url: String,
    @get:JsonProperty("description")
    val description: String?
) : java.io.Serializable

@Serializable
@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.EXISTING_PROPERTY,
    property = "type"
)
sealed class Owner : java.io.Serializable {
    @Serializable
    @JsonTypeName("User")
    data class User(@JsonValue(true) val data: OwnerData) : Owner(), java.io.Serializable {val type = "User"}

    @Serializable
    @JsonTypeName("Organization")
    data class Organization(@JsonValue(true) val data: OrganizationData) : Owner(), java.io.Serializable {val type = "Organization"}
}

@Serializable
data class Repository(
    @get:JsonProperty("id")
    val id: Int,
    @get:JsonProperty("name")
    val name: String,
    @get:JsonProperty("full_name")
    val full_name: String,
    @get:JsonProperty("private")
    val private: Boolean,
    @get:JsonProperty("fork")
    val fork: Boolean,
    @get:JsonProperty("created_at")
    val created_at: String,
    @get:JsonProperty("updated_at")
    val updated_at: String,
    @get:JsonProperty("description")
    val description: String?,
    @get:JsonProperty("owner")
    val owner: Owner,
    @get:JsonProperty("url")
    val url: String,
    @get:JsonProperty("html_url")
    val html_url: String,
    @get:JsonProperty("language")
    val language: String?
) : java.io.Serializable

@Serializable
data class Pusher(
    @get:JsonProperty("name")
    val name: String,
    @get:JsonProperty("email")
    val email: String
) : java.io.Serializable

@Serializable
data class Author(
    @get:JsonProperty("name")
    val name: String,
    @get:JsonProperty("email")
    val email: String,
    @get:JsonProperty("username")
    val username: String
) : java.io.Serializable

@Serializable
data class Label(
    @get:JsonProperty("id")
    val id: Int,
    @get:JsonProperty("url")
    val url: String,
    @get:JsonProperty("name")
    val name: String,
    @get:JsonProperty("color")
    val color: String,
    @get:JsonProperty("default")
    val default: Boolean,
    @get:JsonProperty("description")
    val description: String
) : java.io.Serializable

@Serializable
data class Issue(
    @get:JsonProperty("id")
    val id: Int,
    @get:JsonProperty("url")
    val url: String,
    @get:JsonProperty("html_url")
    val html_url: String,
    @get:JsonProperty("repository_url")
    val repository_url: String,
    @get:JsonProperty("number")
    val number: Int,
    @get:JsonProperty("title")
    val title: String,
    @get:JsonProperty("user")
    val user: UserData,
    @get:JsonProperty("labels")
    val labels: ArrayList<Label>,
    @get:JsonProperty("state")
    val state: String,
    @get:JsonProperty("locked")
    val locked: Boolean,
    @get:JsonProperty("assignee")
    val assignee: UserData?,
    @get:JsonProperty("assignees")
    val assignees: ArrayList<UserData>,
    @get:JsonProperty("comments")
    val comments: Int,
    @get:JsonProperty("created_at")
    val created_at: String,
    @get:JsonProperty("updated_at")
    val updated_at: String,
    @get:JsonProperty("closed_at")
    val closed_at: String?,
    @get:JsonProperty("author_association")
    val author_association: String,
    @get:JsonProperty("body")
    val body: String
) : java.io.Serializable

@Serializable
data class Commit(
    @get:JsonProperty("id")
    val id: String,
    @get:JsonProperty("tree_id")
    val tree_id: String,
    @get:JsonProperty("distinct")
    val distinct: Boolean,
    @get:JsonProperty("message")
    val message: String,
    @get:JsonProperty("timestamp")
    val timestamp: String,
    @get:JsonProperty("url")
    val url: String,
    @get:JsonProperty("author")
    val author: Author,
    @get:JsonProperty("committer")
    val committer: Author,
    @get:JsonProperty("added")
    val added: ArrayList<String>,
    @get:JsonProperty("removed")
    val removed: ArrayList<String>,
    @get:JsonProperty("modified")
    val modified: ArrayList<String>
) : java.io.Serializable

@Serializable
data class PushData(
    @get:JsonProperty("repository")
    val repository: Repository,
    @get:JsonProperty("ref")
    val ref: String,
    @get:JsonProperty("before")
    val before: String,
    @get:JsonProperty("after")
    val after: String,
    @get:JsonProperty("pusher")
    val pusher: Pusher,
    @get:JsonProperty("organization")
    val organization: OrganizationData,
    @get:JsonProperty("sender")
    val sender: UserData,
    @get:JsonProperty("created")
    val created: Boolean,
    @get:JsonProperty("deleted")
    val deleted: Boolean,
    @get:JsonProperty("forced")
    val forced: Boolean,
    @get:JsonProperty("compare")
    val compare: String,
    @get:JsonProperty("commits")
    val commits: ArrayList<Commit>,
    @get:JsonProperty("head_commit")
    val head_commit: Commit
) : java.io.Serializable

@Serializable
@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.EXISTING_PROPERTY,
    property = "type"
)
sealed class WebhookEvent : java.io.Serializable {
    @Serializable
    @JsonTypeName("push")
    data class Push(val data: PushData) : WebhookEvent(), java.io.Serializable {val type = "push"}
}

@Serializable
data class RepositorySearchData(
    @get:JsonProperty("total_count")
    val total_count: Int,
    @get:JsonProperty("incomplete_results")
    val incomplete_results: Boolean,
    @get:JsonProperty("items")
    val items: ArrayList<Repository>
) : java.io.Serializable
}