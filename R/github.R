#' See if there's a new version of the package on GitHub
#'
#' `checkForNewVersion` looks for a tagged version on GitHub and compares with
#' the version number of the currently installed package. `notifyIfNewVersion`
#' wraps it and displays a message if there is a new version. It is called in
#' `.onLoad()`.
#'
#' @param package character package name
#' @param github character "organization/repo" of your package on GitHub
#' @param installed.version character the currently installed version string
#' @return `checkForNewVersion` returns the version string if there is a new
#' version, or NULL. `notifyIfNewVersion` returns nothing invisibly, called for
#' its side effects.
#' @export
#' @keywords internal
notifyIfNewVersion <- function (
    package="crunch",
    github="Crunch-io/rcrunch",
    installed.version=as.character(packageVersion(package))) {

    v <- tryCatch(checkForNewVersion(github, installed.version),
        error=function (e) return(NULL))
    if (!is.null(v)) {
        ## There's a new version. Message:
        message("There's a new version of ", dQuote("crunch"),
            " available. You have ", installed.version, " but ",
            v, " is now available. You can install it with: \n\n",
            'devtools::install_github("', github, '", ref="', v, '")\n\n',
            "You may need to restart R after upgrading.")
    }
    invisible()
}

#' @rdname notifyIfNewVersion
#' @export
#' @keywords internal
#' @importFrom utils compareVersion
checkForNewVersion <- function (github, installed.version) {
    if (getOption("crunch.check.updates", TRUE)) {
        github.url <- paste0("https://api.github.com/repos/", github, "/tags")
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
