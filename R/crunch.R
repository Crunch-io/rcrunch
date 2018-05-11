#' Crunch.io: instant, visual, collaborative data analysis
#'
#' \href{http://crunch.io/}{Crunch.io} provides a cloud-based data store and
#' analytic engine. It has a \href{https://app.crunch.io/}{web client} for
#' interactive data exploration and visualization. The crunch package for R
#' allows analysts to interact with and manipulate Crunch datasets from within
#' R. Importantly, this allows technical researchers to collaborate naturally
#' with team members, managers, and clients who prefer a point-and-click
#' interface: because all connect to the same dataset in the cloud, there is no
#' need to email files back and forth continually to share results.
#'
#' @seealso To learn more about using the package, see
#' \code{vignette("crunch")}. To sign up for a
#' Crunch.io account, visit \url{https://app.crunch.io/}.
#' @docType package
#' @name crunch
NULL

#' @importFrom httr config add_headers
.onLoad <- function (lib, pkgname="crunch") {
    setIfNotAlready(
        crunch.api="https://app.crunch.io/api/",
        httpcache.on=TRUE,
        crunch.namekey.dataset="alias",
        crunch.namekey.array="alias"
    )
    set_crunch_config()
    notifyIfNewVersion()
    invisible()
}

setIfNotAlready <- function (...) {
    newopts <- list(...)
    oldopts <- options()
    oldopts <- oldopts[intersect(names(newopts), names(oldopts))]
    newopts <- modifyList(newopts, oldopts)
    do.call(options, newopts)
    invisible(oldopts)
}
