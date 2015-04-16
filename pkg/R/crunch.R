##' Crunch.io: instant, visual, collaborative data analysis
##'
##' \href{http://crunch.io/}{Crunch.io} provides a cloud-based data store and
##' analytic engine. It has a \href{https://beta.crunch.io/}{web client} for
##' interactive data exploration and visualization. The crunch package for R
##' allows analysts to interact with and manipulate Crunch datasets from within
##' R. Importantly, this allows technical researchers to collaborate naturally
##' with team members, managers, and clients who prefer a point-and-click
##' interface: because all connect to the same dataset in the cloud, there is no
##' need to email files back and forth continually to share results. 
##'
##' @seealso To learn more about using the package, see
##' \code{vignette("getting-started", package="crunch")}. To sign up for a
##' Crunch.io account, visit \url{https://beta.crunch.io/}.
##' @docType package
##' @name crunch
NULL

##' @importFrom httr set_config
.onAttach <- function (lib, pkg="pkg") {
    if (is.null(getOption("crunch.api"))) {
        options(crunch.api="https://beta.crunch.io/api/")
    }
    if (is.null(getOption("crunch.max.categories"))) {
        options(crunch.max.categories=256)
    }
    if (is.null(getOption("crunch.timeout"))) {
        options(crunch.timeout=60)
    }
    if (is.null(getOption("crest.cache"))) {
        options(crest.cache=TRUE)
    }
    options(warn=1)
    set_config(crunchConfig())
    invisible()
}
