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
    categorical_array=function (col, variable) {
        out <- columnParser("categorical")(unlist(col), variable)
        ncols <- length(tuple(variable)$subvariables)
        out <- t(structure(out, .Dim=c(ncols, length(out)/ncols)))
        return(out)
    },
    datetime=function (col, variable) {
        out <- columnParser("text")(col)
        if (all(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", out))) {
            ## return Date if resolution >= D
            return(as.Date(out))
        } else {
            return(as.POSIXct(out))
        }
        ## see http://stackoverflow.com/questions/12125886/parsing-iso8601-in-r to improve
    }
)
columnParser <- function (vartype) {
    return(parse_column[[vartype]] %||% parse_column[["numeric"]])
}

getValues <- function (x, ...) {
    url <- shojiURL(x, "views", "values")
    query <- list(...)
    if (length(query)) {
        return(crGET(url, query=list(...)))
    } else {
        return(crGET(url))
    }
}

##' Convert Variables to local R objects
##'
##' @param x a CrunchVariable subclass
##' @param mode argument not used: part of the generic \code{as.vector}
##' signature
##' @return an R vector of the type corresponding to the Variable. E.g. 
##' CategoricalVariable yields type factor, NumericVariable yields numeric, etc.
##' @name variable-to-R
NULL

##' @rdname variable-to-R
##' @export
setMethod("as.vector", "CrunchVariable", function (x, mode) {
    columnParser(type(x))(getValues(x), x)
})

