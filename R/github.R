notifyIfNewVersion <- function (
    github.url="https://api.github.com/repos/Crunch-io/rcrunch/tags",
    installed.version=as.character(packageVersion("crunch"))) {

    mc <- match.call()
    mc[[1L]] <- quote(checkForNewVersion)

    v <- try(eval.parent(mc), silent=TRUE)
    if (!is.error(v) && is.character(v)) {
        ## There's a new version. Message:
        message("There's a new version of ", dQuote("crunch"),
            " available. You have ", installed.version, " but ",
            v, " is now available. You can install it with: \n\n",
            'devtools::install_github("Crunch-io/rcrunch", ref="', v, '")\n\n',
            "You may need to restart R after upgrading.")
    }
    invisible()
}

#' See if there's a new version of the package on GitHub
#'
#' @param github.url character where to GET the tagged versions of the package
#' @param installed.version character the currently installed version string
#' @return The version string if there is a new version, or NULL
#' @importFrom utils compareVersion
#' @export
#' @keywords internal
checkForNewVersion <- function (
    github.url="https://api.github.com/repos/Crunch-io/rcrunch/tags",
    installed.version=as.character(packageVersion("crunch"))) {

    if (getOption("crunch.check.updates") %||% TRUE) {
        ## Get the names of the tagged versions on GitHub
        gh.tags <- vapply(crGET(github.url), vget("name"), character(1))
        ## Filter to keep only those that match x.y.z
        version.tags <- grep("^[0-9]+\\.[0-9]+\\.[0-9]+$", gh.tags, value=TRUE)
        if (length(version.tags) &&
            compareVersion(version.tags[1], installed.version) == 1) {
            return(version.tags[1])
        }
    }
    return(NULL)
}
