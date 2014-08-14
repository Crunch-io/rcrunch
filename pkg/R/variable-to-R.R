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

##' @export
setMethod("as.vector", "CrunchVariable", function (x, mode) {
    columnParser(type(x))(getValues(x), x)
})

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
