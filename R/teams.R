##' Teams
##'
##' Teams contain users and datasets. You can share a dataset with a group of
##' users by sharing the dataset with a team. You can also share a bunch of
##' datasets with a user all at once by adding them to a team that has those
##' datasets.
##'
##' These methods allow you to work with teams. Find your teams with the
##' \code{\link{getTeams}} function, which returns your \code{TeamCatalog}.
##' Extract an individual team by name. Create a team by assigning in with a new
##' name, with the assignment value a list, either empty (to just create a team
##' with that name), or with a "members" element, containing emails or URLs of
##' users to add to the team. Users can be added later with the \code{members<-}
##' method.
##'
##' @param x a \code{CrunchTeam}
##' @param value for \code{members<-}, a character vector of emails or URLs of
##' users to add to the team.
##' @return \code{members} returns a
##' \code{MemberCatalog}, which has references to the users that are members
##' of the team. \code{members<-} returns \code{x} with the given users added
##' to the members catalog.
##' @aliases members members<-
##' @seealso \link{getTeams}
##' @name teams
NULL

##' Retrive all teams you're a member of
##'
##' @return A \code{TeamCatalog}. Extract an individual team by name. Create
##' a team by assigning in with a new name.
##' @seealso \link{teams}
##' @export
getTeams <- function () {
    TeamCatalog(crGET(sessionURL("teams")))
}

##' @rdname catalog-extract
##' @export
setMethod("[[", c("TeamCatalog", "character"), function (x, i, ...) {
    stopifnot(length(i) == 1)
    z <- match(i, names(x))
    if (is.na(z)) {
        return(NULL)
    }
    return(x[[z]])
})

##' @rdname catalog-extract
##' @export
setMethod("[[", c("TeamCatalog", "numeric"), function (x, i, ...) {
    stopifnot(length(i) == 1)
    url <- urls(x)[i]
    return(CrunchTeam(crGET(url)))
})

##' @rdname catalog-extract
##' @export
setMethod("[[<-", c("TeamCatalog", "character", "missing", "list"),
    function (x, i, j, value) {
        if (i %in% names(x)) {
            ## TODO: update team attributes
            halt("Cannot (yet) modify team attributes")
        } else {
            ## Creating a new team
            u <- crPOST(self(x), body=toJSON(list(name=i)))
            x <- refresh(x)
            ## Add members to team, if given
            if (!is.null(value[["members"]]))
            members(x[[i]]) <- value[["members"]]
            return(x)
        }
    })

##' @rdname catalog-extract
##' @export
setMethod("[[<-", c("TeamCatalog", "character", "missing", "CrunchTeam"),
    function (x, i, j, value) {
        ## TODO: something
        ## For now, assuming that modifications have already been persisted
        ## by other operations on the team entity (like members<-)
        return(x)
    })

##' @rdname catalog-extract
##' @export
setMethod("$", "TeamCatalog", function (x, name) x[[name]])

##' @rdname describe
##' @export
setMethod("name", "CrunchTeam", function (x) x@body$name)

##' @rdname teams
##' @export
setMethod("members", "CrunchTeam", function (x) {
    MemberCatalog(crGET(shojiURL(x, "catalogs", "members")))
})

.backstopUpdate <- function (x, i, j, value) {
    ## Backstop error so you don't get "Object of class S4 is not subsettable"
    halt(paste("Cannot update", class(x), "with type", class(value)))
}

##' @rdname catalog-extract
##' @export
setMethod("[[<-", c("MemberCatalog", "ANY", "missing", "ANY"), .backstopUpdate)

##' @rdname catalog-extract
##' @export
setMethod("[[<-", c("MemberCatalog", "character", "missing", "NULL"),
    function (x, i, j, value) {
        ## Remove the specified user from the catalog
        payload <- sapply(i, function (z) NULL, simplify=FALSE)
        crPATCH(self(x), body=toJSON(payload))
        return(refresh(x))
    })

##' @rdname teams
##' @export
setMethod("members<-", c("CrunchTeam", "MemberCatalog"), function (x, value) {
    ## TODO: something
    ## For now, assume action already done in other methods, like NULL
    ## assignment above.
    return(x)
})

##' @rdname teams
##' @export
setMethod("members<-", c("CrunchTeam", "character"), function (x, value) {
    payload <- sapply(value,
        function (z) emptyObject(),
        simplify=FALSE)
    crPATCH(self(members(x)), body=toJSON(payload))
    return(refresh(x))
})

##' @rdname delete
##' @export
setMethod("delete", "CrunchTeam", function (x, confirm=requireConsent(), ...) {
    prompt <- paste0("Really delete team ", dQuote(name(x)), "? ",
        "This cannot be undone.")
    if (confirm && !askForPermission(prompt)) {
        halt("Must confirm deleting team")
    }
    u <- self(x)
    out <- crDELETE(u)
    dropCache(absoluteURL("../", u))
    invisible(out)
})
