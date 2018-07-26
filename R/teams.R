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
setMethod(
    "[[<-", c("TeamCatalog", "character", "missing", "list"),
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
setMethod(
    "[[<-", c("TeamCatalog", "character", "missing", "CrunchTeam"),
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

#' @rdname delete
#' @export
setMethod("delete", "CrunchTeam", function(x, ...) {
    prompt <- paste0(
        "Really delete team ", dQuote(name(x)), "? ",
        "This cannot be undone."
    )
    if (!askForPermission(prompt)) {
        halt("Must confirm deleting team")
    }
    u <- self(x)
    out <- crDELETE(u)
    dropCache(absoluteURL("../", u))
    invisible(out)
})
