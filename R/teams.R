#' Teams
#'
#' Teams contain users and datasets. You can share a dataset with a group of
#' users by sharing the dataset with a team. You can also share a set of
#' datasets with a user all at once by adding the user to a team that contains those
#' datasets.
#'
#' These methods allow you to work with teams. Find your teams with the
#' [getTeams()] function, which returns your `TeamCatalog`. You can extract an individual team by name,
#' or create a team by assigning into the function. To create a team by assignment, assign a list
#' to `teams("myteam") <- value_list`, the `value_list` can either empty (to just create a team
#' with that name), or can contain a "members" element with the emails or URLs of
#' users to add to the team. Users can be also be added later with the `members<-`
#' method.
#'
#' @param x a `CrunchTeam`
#' @param value for `members<-`, a character vector of emails or URLs of
#' users to add to the team.
#' @return `members` returns a
#' `MemberCatalog`, which has references to the users that are members
#' of the team. `members<-` returns `x` with the given users added
#' to the members catalog.
#' @aliases members members<-
#' @seealso [`getTeams`]
#' @name teams
NULL

#' Retrieve your teams
#'
#' @return A `TeamCatalog`. Extract an individual team by name. Create
#' a team by assigning in with a new name.
#' @seealso [`teams`]
#' @export
getTeams <- function() {
    TeamCatalog(crGET(sessionURL("teams")))
}

#' @rdname catalog-extract
#' @export
setMethod("[[", c("TeamCatalog", "numeric"), function(x, i, ...) {
    getEntity(x, i, CrunchTeam, ...)
})

#' @rdname catalog-extract
#' @export
setMethod("[[<-", c("TeamCatalog", "character", "missing", "list"),
    function(x, i, j, value) {
        if (i %in% names(x)) {
            ## TODO: update team attributes
            halt("Cannot (yet) modify team attributes")
        } else {
            ## Creating a new team
            u <- crPOST(self(x), body = toJSON(list(name = i)))
            x <- refresh(x)
            ## Add members to team, if given
            if (!is.null(value[["members"]])) {
                members(x[[i]]) <- value[["members"]]
            }
            return(x)
        }
    }
)

#' @rdname catalog-extract
#' @export
setMethod("[[<-", c("TeamCatalog", "character", "missing", "CrunchTeam"),
    function(x, i, j, value) {
        ## TODO: something
        ## For now, assuming that modifications have already been persisted
        ## by other operations on the team entity (like members<-)
        return(x)
    }
)

#' @rdname teams
#' @export
setMethod("members", "CrunchTeam", function(x) {
    MemberCatalog(crGET(shojiURL(x, "catalogs", "members")))
})

#' Share Crunch assets with a team
#'
#' You can share filters and multitables with a team that you are on. This will
#' give all team members access to view and edit these filters. Use `getTeams()`
#' to see what teams you are on.
#'
#' @param x a `CrunchFilter` or `Multitable`
#' @param value a `CrunchTeam` or url for a Crunch team
#'
#' @return a `CrunchTeam` that the asset is shared with.
#'
#' @name team-sharing
#' @export
setGeneric("team", function(x) standardGeneric("team"))

.getTeam <- function(x) {
    if (is.null(x@body$team)) {
        return(NULL)
    }
    return(CrunchTeam(crGET(x@body$team)))
}

#' @rdname team-sharing
#' @export
setMethod("team", "CrunchFilter", .getTeam)

#' @rdname team-sharing
#' @export
setMethod("team", "Multitable", .getTeam)

#' @rdname team-sharing
#' @export
setMethod("team", "CrunchDeck", .getTeam)

#' @rdname team-sharing
#' @export
setGeneric("team<-", function(x, value) standardGeneric("team<-"))

.setTeam <- function(x, value) {
    if (inherits(value, "CrunchTeam")) {
        value <- self(value)
    }
    if (!(is.crunchURL(value) || is.null(value))) {
        halt("Team setting requires either a CrunchTeam entity, URL, or NULL")
    }
    return(setEntitySlot(x, "team", value))
}
#' @rdname team-sharing
#' @export
setMethod("team<-", c("CrunchFilter", "ANY"), .setTeam)

#' @rdname team-sharing
#' @export
setMethod("team<-", c("Multitable", "ANY"), .setTeam)

#' @rdname team-sharing
#' @export
setMethod("team<-", c("CrunchDeck", "ANY"), .setTeam)
