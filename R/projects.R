#' Get the project catalog
#'
#' @param x a `ShojiObject` that has an associated project catalog. If omitted,
#' the default value for `x` means that you will load the user's primary
#' project catalog. (Currently, there are no other project catalogs to load.)
#' @return An object of class `ProjectCatalog`.
#' @name projects
#' @export
#' @examples
#' \dontrun{
#' myprojects <- projects()
#' proj <- myprojects[["Project name"]]
#' }
projects <- function(x = getAPIRoot()) {
    cont <- crGET(shojiURL(x, "catalogs", "projects"))
    if (new_projects_api()) {
        return(ProjectFolder(cont))
    } else {
        return(ProjectCatalog(cont))
    }
}

new_projects_api <- function () {
    # TODO can feature flag this with R options too
    !featureFlag("old_projects_order")
}

is.project <- function (x) inherits(x, c("CrunchProject", "ProjectFolder"))

#' @rdname catalog-extract
#' @export
setMethod("[[", c("ProjectCatalog", "numeric"), function(x, i, ...) {
    getTuple(x, i, CrunchProject)
})

#' @rdname catalog-extract
#' @export
setMethod(
    "[[<-", c("ProjectCatalog", "character", "missing", "list"),
    function(x, i, j, value) {
        if (i %in% names(x)) {
            ## TODO: update team attributes
            halt("Cannot (yet) modify project attributes")
        } else {
            ## Creating a new project
            proj <- do.call(
                newProject,
                modifyList(value, list(name = i, catalog = x))
            )
            return(refresh(x))
        }
    }
)

#' Create a new project
#'
#' This function creates a new project. You can achieve the same results by
#' assigning into the projects catalog, but this may be a more natural way to
#' think of the action, particularly when you want to do something with the
#' project entity after you create it.
#' @param name character name for the project
#' @param members Optional character vector of emails or user URLs to add as
#' project members.
#' @param catalog ProjectCatalog in which to create the new project. There is
#' only one project catalog currently, [projects()], but this is left here
#' so that all `new*` functions follow the same pattern.
#' @param ... Additional project attributes to set
#' @return A `CrunchProject` object.
#' @examples
#' \dontrun{
#' proj <- newProject("A project name")
#' # That is equivalent to doing:
#' p <- projects()
#' p[["A project name"]] <- list()
#' proj <- p[["A project name"]]
#'
#' proj2 <- newProject("Another project", members="you@yourco.com")
#' # That is equivalent to doing:
#' p[["Another project"]] <- list(members="you@yourco.com")
#' proj <- p[["Another project"]]
#' }
#' @export
newProject <- function(name, members = NULL, catalog = projects(), ...) {
    u <- crPOST(self(catalog), body = toJSON(list(name = name, ...)))
    ## Fake a CrunchProject (tuple) by getting the entity
    ## TODO: make this more robust and formal (useful elsewhere too?)
    out <- CrunchProject(
        index_url = self(catalog), entity_url = u,
        body = crGET(u)$body
    )
    ## Add members to project, if given
    if (!is.null(members)) {
        members(out) <- members
    }
    invisible(out)
}

#' @rdname catalog-extract
#' @export
setMethod(
    "[[<-", c("ProjectCatalog", "character", "missing", "CrunchProject"),
    function(x, i, j, value) {
        ## Assumes that modifications have already been persisted
        ## by other operations on the team entity (like members<-)
        index(x)[[value@entity_url]] <- value@body
        return(x)
    }
)

#' @rdname teams
#' @export
setMethod("members", "CrunchProject", function(x) {
    MemberCatalog(crGET(shojiURL(x, "catalogs", "members")))
})

#' @rdname teams
#' @export
setMethod("members<-", c("CrunchProject", "MemberCatalog"), function(x, value) {
    ## TODO: something
    ## For now, assume action already done in other methods, like NULL
    ## assignment above.
    return(x)
})

#' @rdname teams
#' @export
setMethod("members<-", c("CrunchProject", "character"), function(x, value) {
    value <- setdiff(value, emails(members(x)))
    if (length(value)) {
        payload <- sapply(value, emptyObject, simplify = FALSE)
        crPATCH(self(members(x)), body = toJSON(payload))
    }
    return(x)
})

#' @rdname tuple-methods
#' @export
setMethod("entity", "CrunchProject", function(x) {
    return(ProjectEntity(crGET(x@entity_url)))
})

#' @rdname delete
#' @export
setMethod("delete", "CrunchProject", function(x, ...) {
    prompt <- paste0(
        "Really delete project ", dQuote(name(x)), "? ",
        "This cannot be undone."
    )
    if (!askForPermission(prompt)) {
        halt("Must confirm deleting project")
    }
    u <- self(x)
    out <- crDELETE(u)
    dropCache(absoluteURL("../", u))
    invisible(out)
})

#' @rdname datasets
#' @export
`datasets<-` <- function(x, value) {
    stopifnot(is.project(x))
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

#' Get or set a project's icon
#'
#' This function allows you to change a project's icon. Note that you
#' must first upload the icon to a website, as this function does not
#' allow you to upload a local file.
#' @param x a `CrunchProject`
#' @param value character file path of the icon image file to set
#' @return The URL of the project's icon. The setter returns the
#' project after having uploaded the specified file as the new icon.
#' @name project-icon
#' @export
icon <- function(x) {
    stopifnot(is.project(x))
    return(x@body$icon)
}

#' @rdname project-icon
#' @export
`icon<-` <- function(x, value) {
    crPUT(shojiURL(x, "views", "icon"),
        body = list(icon = upload_file(value))
    )
    dropOnly(absoluteURL("../", self(x))) ## Invalidate catalog
    return(refresh(x))
}
