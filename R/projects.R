##' Get the project catalog
##'
##' @param x a \code{ShojiObject} that has a project catalog associated. If omitted,
##' the default value for \code{x} means that you will load the user's primary
##' project catalog. (Currently, there are no other project catalogs to load.)
##' @return An object of class \code{ProjectCatalog}.
##' @name projects
##' @export
##' @examples
##' \dontrun{
##' myprojects <- projects()
##' proj <- myprojects[["Project name"]]
##' }
projects <- function (x=getAPIRoot()) {
    ProjectCatalog(crGET(shojiURL(x, "catalogs", "projects")))
}

##' @rdname catalog-extract
##' @export
setMethod("[[", c("ProjectCatalog", "character"), function (x, i, ...) {
    w <- whichNameOrURL(x, i)
    x[[w]]
})

##' @rdname catalog-extract
##' @export
setMethod("[[", c("ProjectCatalog", "ANY"), function (x, i, ...) {
    b <- callNextMethod(x, i, ...)
    if (is.null(b)) return(NULL)
    CrunchProject(index_url=self(x), entity_url=urls(x)[i],
        body=b)
})

##' @rdname catalog-extract
##' @export
setMethod("[[<-", c("ProjectCatalog", "character", "missing", "list"),
    function (x, i, j, value) {
        if (i %in% names(x)) {
            ## TODO: update team attributes
            halt("Cannot (yet) modify project attributes")
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
setMethod("[[<-", c("ProjectCatalog", "character", "missing", "CrunchProject"),
    function (x, i, j, value) {
        ## Assumes that modifications have already been persisted
        ## by other operations on the team entity (like members<-)
        index(x)[[value@entity_url]] <- value@body
        return(x)
    })

##' @rdname teams
##' @export
setMethod("members", "CrunchProject", function (x) {
    MemberCatalog(crGET(shojiURL(x, "catalogs", "members")))
})

##' @rdname teams
##' @export
setMethod("members<-", c("CrunchProject", "MemberCatalog"), function (x, value) {
    ## TODO: something
    ## For now, assume action already done in other methods, like NULL
    ## assignment above.
    return(x)
})

##' @rdname teams
##' @export
setMethod("members<-", c("CrunchProject", "character"), function (x, value) {
    value <- setdiff(value, emails(members(x)))
    if (length(value)) {
        payload <- sapply(value, emptyObject, simplify=FALSE)
        crPATCH(self(members(x)), body=toJSON(payload))
    }
    return(x)
})

##' @rdname tuple-methods
##' @export
setMethod("entity", "CrunchProject", function (x) {
    return(ProjectEntity(crGET(x@entity_url)))
})

##' @rdname delete
##' @export
setMethod("delete", "CrunchProject", function (x, confirm=requireConsent(), ...) {
    prompt <- paste0("Really delete project ", dQuote(name(x)), "? ",
        "This cannot be undone.")
    if (confirm && !askForPermission(prompt)) {
        halt("Must confirm deleting project")
    }
    u <- self(x)
    out <- crDELETE(u)
    dropCache(absoluteURL("../", u))
    invisible(out)
})

##' @rdname datasets
##' @export
`datasets<-` <- function (x, value) {
    stopifnot(inherits(x, "CrunchProject"))
    if (is.dataset(value)) {
        ## This is how we add a dataset to a project: change its owner
        owner(value) <- x
        dropCache(shojiURL(x, "catalogs", "datasets"))
    }
    ## Else, we're doing something like `ordering(datasets(proj)) <- `
    ## and no action is required.
    ## TODO: setmethods for this. This feels fragile.
    return(x)
}

##' A project's icon
##' @param x a \code{CrunchProject}
##' @param value charcter file path of the icon image file to set
##' @return The URL of the project's icon. The setter returns the
##' project after having uploaded the specified file as the new icon.
##' @name project-icon
##' @export
icon <- function (x) {
    stopifnot(inherits(x, "CrunchProject"))
    return(x@body$icon)
}

##' @rdname project-icon
##' @export
`icon<-` <- function (x, value) {
    crPUT(shojiURL(x, "views", "icon"),
        body=list(icon=upload_file(value)))
    dropOnly(absoluteURL("../", self(x))) ## Invalidate catalog
    return(refresh(x))
}
