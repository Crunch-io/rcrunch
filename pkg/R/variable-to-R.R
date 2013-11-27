getValues <- function (x, ...) {
    url <- x@urls$values_url
    query <- list(...)
    if (length(query)) {
        return(GET(url, query=list(...)))
    } else {
        return(GET(url))
    }
    
}

##' as.vector methods for Crunch variables
##' @export 
##' @S3method as.vector CrunchVariable
as.vector.CrunchVariable <- function (x, mode) getValues(x)

##' @S3method as.vector CategoricalVariable
as.vector.CategoricalVariable <- function (x, mode) {
    out <- as.vector.CrunchVariable(x)
    ## If sending ids:
    # out <- as.factor(names(categories(x))[match(out, ids(categories(x)))])
    ## If sending category names
    out <- factor(out, levels=names(categories(x)))
    return(out)
}

##' @S3method as.vector NumericVariable
as.vector.NumericVariable <- function (x, mode) {
    out <- as.vector.CrunchVariable(x)
    missings <- vapply(out, Negate(is.numeric), logical(1))
    out[missings] <- NA_real_
    return(as.numeric(unlist(out)))
}

##' @S3method as.vector TextVariable
as.vector.TextVariable <- function (x, mode) {
    out <- as.vector.CrunchVariable(x)
    missings <- vapply(out, Negate(is.character), logical(1))
    out[missings] <- NA_character_
    return(as.character(unlist(out)))
}

##' @S3method as.vector DatetimeVariable
as.vector.DatetimeVariable <- function (x, mode) {
    out <- as.vector.TextVariable(x) ## Because that's how they come across the wire
    return(as.POSIXct(out))
    ## see http://stackoverflow.com/questions/12125886/parsing-iso8601-in-r to improve
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
