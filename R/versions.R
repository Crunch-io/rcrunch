init.VersionCatalog <- function (.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    ord <- order(from8601(getIndexSlot(.Object, "last_update")),
        decreasing=TRUE)
    .Object@index <- .Object@index[ord]
    return(.Object)
}
setMethod("initialize", "VersionCatalog", init.VersionCatalog)

versions <- function (x) {
    stopifnot(is.dataset(x))
    return(VersionCatalog(crGET(shojiURL(x, "catalogs", "savepoints"))))
}

##' @rdname describe-catalog
##' @export
setMethod("names", "VersionCatalog", function (x) getIndexSlot(x, "description"))

##' @rdname describe-catalog
##' @export
setMethod("descriptions", "VersionCatalog", function (x) getIndexSlot(x, "description"))

##' @rdname describe-catalog
##' @export
setMethod("timestamps", "VersionCatalog", function (x) from8601(getIndexSlot(x, "last_update")))

showVersionCatalog <- function (x, from=Sys.time()) {
    ts <- timestamps(x)
    if (!is.null(from)) {
        ts <- vapply(seq_along(ts), function (a) {
            ## Grab dates by sequence because POSIXt is a list internally
            ## (i.e. lapply does the wrong thing)
            this <- from - ts[a]
            num <- as.integer(this)
            un <- attr(this, "units")
            if (num == 1) {
                ## Make singular
                un <- sub("s$", "", un)
            }
            out <- paste(num, un, "ago")
            return(out)
        }, character(1))
    }
    return(data.frame(Name=names(x), Timestamp=ts, stringsAsFactors=FALSE))
}

##' @rdname show-crunch
##' @export
setMethod("show", "VersionCatalog", function (object) {
    out <- showVersionCatalog(object)
    print(out)
    invisible(out)
})

saveVersion <- function (dataset, description=NULL) {
    u <- shojiURL(dataset, "catalogs", "savepoints")
    out <- crPOST(u, body=toJSON(list(description=description)))
    invisible(out)
}

restoreVersion <- function (dataset, version) {
    vcat <- versions(dataset)
    if (is.numeric(version)) {
        ## Allow passing by index
        v <- version
    } else {
        v <- which(names(vcat) == version)
        if (length(v) == 0) {
            halt(dQuote(version), " does not match any available versions")
        } else if (length(v) > 1) {
            halt(dQuote(version), " matches more than one version. Cannot restore.")
        }
    }
    revert_url <- index(vcat)[[v]]$revert
    crPOST(revert_url, body=NULL)
    invisible(refresh(dataset))
}