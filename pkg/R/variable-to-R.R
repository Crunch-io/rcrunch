parse_column <- list(
    numeric=function (col, variable) {
        missings <- vapply(col, Negate(is.numeric), logical(1))
        col[missings] <- NA_real_
        return(as.numeric(unlist(col)))
    },
    text=function (col, variable) {
        missings <- vapply(col, Negate(is.character), logical(1))
        col[missings] <- NA_character_
        return(as.character(unlist(col)))
    },
    categorical=function (col, variable) {
        out <- columnParser("numeric")(col)
        cats <- na.omit(categories(variable))
        out <- factor(names(cats)[match(out, ids(cats))], levels=names(cats))
        return(out)
    },
    datetime=function (col, variable) {
        out <- columnParser("text")(col)
        return(as.POSIXct(out)) ## return Date if resolution >= D?
        ## see http://stackoverflow.com/questions/12125886/parsing-iso8601-in-r to improve
    }
)
columnParser <- function (vartype) {
    return(parse_column[[vartype]] %||% parse_column[["numeric"]])
}

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
##' @S3method as.vector CrunchVariable
as.vector.CrunchVariable <- function (x, mode) getValues(x)

##' @S3method as.vector NumericVariable
as.vector.NumericVariable <- function (x, mode) {
    columnParser("numeric")(as.vector.CrunchVariable(x))
}

##' @S3method as.vector TextVariable
as.vector.TextVariable <- function (x, mode) {
    columnParser("text")(as.vector.CrunchVariable(x))
}

##' @S3method as.vector CategoricalVariable
as.vector.CategoricalVariable <- function (x, mode) {
    columnParser("categorical")(as.vector.CrunchVariable(x), x)
}

##' @S3method as.vector DatetimeVariable
as.vector.DatetimeVariable <- function (x, mode) {
    columnParser("datetime")(as.vector.CrunchVariable(x))
}

##' as.data.frame method for CrunchDataset
##'
##' @rdname dataset-to-R
##' @export
as.data.frame.CrunchDataset <- function (x, row.names = NULL, optional = FALSE, ...) {
    default.stringsAsFactors <- function () FALSE
    out <- lapply(x, as.vector)
    names(out) <- names(x)
    return(as.data.frame(out, ...))
}
