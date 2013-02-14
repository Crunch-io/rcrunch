validCrunchDataset <- function (object) {
    oname <- object@body$name
    are.vars <- vapply(object, is.variable, logical(1))
    if (!all(are.vars)) {
        badcount <- sum(!are.vars)
        val <- paste0("Invalid dataset ", sQuote(oname), ": ", badcount, 
            ifelse(badcount>1, 
                " elements are not Crunch variable objects.", 
                " element is not a Crunch variable object."))
    } else {
        val <- TRUE  
    }
    return(val)
}
setValidity("CrunchDataset", validCrunchDataset)

##' Is it?
##' @rdname crunch-is
##' @param x an object
##' @return logical
##' @export 
is.dataset <- function (x) inherits(x, "CrunchDataset")

setGeneric("name", function (x) standardGeneric("name"))
setMethod("name", "CrunchDataset", function (x) x@body$name)

.cr.dataset.shojiObject <- function (x, ...) {
    out <- CrunchDataset(x, ...)
    if (length(list(...))==0) {
        ## get variables
        urls <- out@urls$variables_url
        if (!is.null(urls)) {
            vars <- getShojiCollection(urls, "body$alias")
            out@.Data <- lapply(vars, as.variable)
        }
    }
    return(out)
}

setAs("ShojiObject", "CrunchDataset", 
    function (from) .cr.dataset.shojiObject(from))
setAs("shoji", "CrunchDataset", 
    function (from) as(as.shojiObject(from), "CrunchDataset"))

as.dataset <- function (x) as(x, "CrunchDataset")

is.readonly <- function (x) isTRUE(x@readonly)

setMethod("[", c("CrunchDataset", "ANY"), function (x, i, ..., drop=FALSE) {
    x@.Data <- x@.Data[i]
    x@readonly <- TRUE ## we don't want to overwrite the big object accidentally
    return(x)
})