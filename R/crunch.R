#' @keywords internal
"_PACKAGE"

#' @importFrom httr config add_headers
.onLoad <- function(lib, pkgname = "crunch") {
    setIfNotAlready(
        httpcache.on = TRUE,
        crunch.api = "https://app.crunch.io/api/",
        crunch.namekey.dataset = "alias",
        crunch.namekey.array = "alias"
    )
    set_crunch_config()
    notifyIfNewVersion()
    invisible()
}

setIfNotAlready <- function(...) {
    newopts <- list(...)
    oldopts <- options()
    oldopts <- oldopts[intersect(names(newopts), names(oldopts))]
    newopts <- modifyList(newopts, oldopts)
    do.call(options, newopts)
    invisible(oldopts)
}
