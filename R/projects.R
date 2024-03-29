#' List project folders
#'
#' @param x a `ShojiObject` that has an associated catalog. If omitted,
#' the default value for `x` means that you will load the user's primary
#' folder. (Currently, there are no other folders to load.)
#' @return An object of class `ProjectFolder`.
#' @name projects
#' @export
#' @examples
#' \dontrun{
#' myprojects <- projects()
#' proj <- myprojects[["Project name"]]
#' }
projects <- function(x = getAPIRoot()) {
    ProjectFolder(crGET(shojiURL(x, "catalogs", "projects")))
}

is.project <- function(x) inherits(x, "ProjectFolder")

#' Create a new folder
#'
#' This function creates a new project. You can achieve the same results by
#' assigning into the projects catalog, but this may be a more natural way to
#' think of the action, particularly when you want to do something with the
#' project entity after you create it.
#' @param name character name for the project
#' @param members Optional character vector of emails or user URLs to add as
#' project members.
#' @param catalog ProjectFolder in which to create the new project. There is
#' only one project catalog currently, [projects()], but this is left here
#' so that all `new*` functions follow the same pattern.
#' @param ... Additional project attributes to set
#' @return A `ProjectFolder` object.
#' @examples
#' \dontrun{
#' proj <- newProject("A project name")
#' # That is equivalent to doing:
#' p <- projects()
#' p[["A project name"]] <- list()
#' proj <- p[["A project name"]]
#'
#' proj2 <- newProject("Another project", members = "you@yourco.com")
#' # That is equivalent to doing:
#' p[["Another project"]] <- list(members = "you@yourco.com")
#' proj <- p[["Another project"]]
#' }
#' @export
#' @seealso [mkdir()]
newProject <- function(name, members = NULL, catalog = projects(), ...) {
    out <- cd(catalog, name, create = TRUE)
    ## Add members to project, if given
    if (!is.null(members)) {
        members(out) <- members
    }
    invisible(out)
}

#' @rdname datasets
#' @export
`datasets<-` <- function(x, value) {
    stopifnot(is.project(x))
    .moveToFolder(x, value)
}
