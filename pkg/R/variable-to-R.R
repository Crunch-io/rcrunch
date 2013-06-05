getValues <- function (x, ...) {
    url <- x@urls$values_url
    GET(url, query=list(...))
}

##' as.vector methods for Crunch variables
##' @export 
##' @S3method as.vector CrunchVariable
as.vector.CrunchVariable <- function (x, mode) getValues(x)

##' @S3method as.vector CategoricalVariable
as.vector.CategoricalVariable <- function (x, mode) {
    out <- factor(as.vector.CrunchVariable(x))
    return(out)
}

## Left here in case we prefer keeping it S4. But as.data.frame apparently 
## needs to be S3, otherwise model.frame (e.g.) won't find it...
# setMethod("as.vector", "CrunchVariable", function (x, mode) getValues(x))
# setMethod("as.vector", "CategoricalVariable", function (x, mode) {
#     out <- callNextMethod()
#     out <- factor(out)
#     return(out)
# })

##' @S3method as.data.frame CrunchDataset
as.data.frame.CrunchDataset <- function (x, row.names = NULL, optional = FALSE, ...) {
    default.stringsAsFactors <- function () FALSE
    out <- lapply(x, as.vector)
    return(as.data.frame(out, ...))
}
