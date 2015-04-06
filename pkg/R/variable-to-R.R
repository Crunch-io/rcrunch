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

##' as.data.frame method for CrunchDataset
##'
##' @param x a CrunchDataset
##' @param row.names part of as.data.frame signature. Ignored.
##' @param optional part of as.data.frame signature. Ignored.
##' @param ... additional arguments passed to as.data.frame.default
##' @return a data.frame
##' @name dataset-to-R
NULL

##' @rdname dataset-to-R
##' @export
as.data.frame.CrunchDataset <- function (x, row.names = NULL, optional = FALSE, ...) {
    default.stringsAsFactors <- function () FALSE
    limit <- min(c(10000, getOption("crunch.data.frame.limit")))
    if (nrow(x) * ncol(x) > limit) {
        halt("Dataset too large to coerce to data.frame. ",
            "Consider subsetting it first")
    }
    out <- lapply(x, as.vector)
    names(out) <- names(x)
    # return(as.data.frame(out, ...))
    return(structure(out, class="data.frame", row.names=c(NA, -nrow(x))))
}
