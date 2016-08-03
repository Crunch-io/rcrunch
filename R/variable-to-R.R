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
    categorical_ids=function (col, variable) {
        missings <- vapply(col, is.list, logical(1)) ## for the {?:values}
        col[missings] <- lapply(col[missings], function (x) x[["?"]])
        return(as.numeric(unlist(col)))
    },
    categorical_numeric_values=function (col, variable) {
        out <- columnParser("numeric")(col)
        cats <- na.omit(categories(variable))
        out <- values(cats)[match(out, ids(cats))]
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
        return(from8601(out))
    }
)
columnParser <- function (vartype, mode=NULL) {
    if (vartype == "categorical") {
        ## Deal with mode. Valid modes: factor (default), numeric, id
        if (!is.null(mode)) {
            if (mode == "numeric") {
                vartype <- "categorical_numeric_values"
            } else if (mode == "id") {
                vartype <- "categorical_ids" ## The numeric parser will return ids, right?
            }
        }
    }
    return(parse_column[[vartype]] %||% parse_column[["numeric"]])
}

getValues <- function (x, ...) {
    paginatedGET(shojiURL(x, "views", "values"), list(...))
}

paginatedGET <- function (url, query, offset=0,
                          limit=getOption("crunch.page.size") %||% 1000,
                          table=FALSE) {
    ## Paginate the GETting of values. Called both from getValues and in
    ## the as.vector.CrunchExpr method in expressions.R

    query$offset <- offset
    query$limit <- limit

    out <- list()
    keep.going <- TRUE
    i <- 1

    ## Function to determine number of values received, depending on whether
    ## we have a crunch:table or shoji:view
    if (table) {
        len <- function (x) length(x$data$out)
    } else {
        len <- length
    }
    with(temp.option(scipen=15), {
        ## Mess with scipen so that the query string formatter doesn't
        ## convert an offset like 100000 to '1+e05', which server rejects
        while(keep.going) {
            ## Wrap the GET in a parser function, default no-op, so we can
            ## get data out of a crunch:table
            out[[i]] <- crGET(url, query=query)
            if (len(out[[i]]) < limit) {
                keep.going <- FALSE
            } else {
                query$offset <- query$offset + limit
                i <- i + 1
            }
        }
    })

    ## Collect the result
    if (table) {
        out[[1]]$data$out <- unlist(lapply(out, function (x) x$data$out),
            recursive=FALSE)
        out <- out[[1]]
    } else {
        out <- unlist(out, recursive=FALSE)
    }
    return(out)
}

#' Convert Variables to local R objects
#'
#' @param x a CrunchVariable subclass
#' @param mode for Categorical variables, one of either "factor" (default,
#' which returns the values as factor); "numeric" (which returns the numeric
#' values); or "id" (which returns the category ids). If "id", values
#' corresponding to missing categories will return as the underlying integer
#' codes; i.e., the R representation will not have any \code{NA}s. Otherwise,
#' missing categories will all be returned \code{NA}. For non-Categorical
#' variables, the \code{mode} argument is ignored.
#' @return an R vector of the type corresponding to the Variable. E.g.
#' CategoricalVariable yields type factor by default, NumericVariable yields
#' numeric, etc.
#' @name variable-to-R
NULL

#' @rdname variable-to-R
#' @export
setMethod("as.vector", "CrunchVariable", function (x, mode) {
    f <- zcl(activeFilter(x))
    columnParser(type(x), mode)(getValues(x, filter=toJSON(f)), x)
})

from8601 <- function (x) {
    ## Crunch timestamps look like "2015-02-12T10:28:05.632000+00:00"

    if (all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", na.omit(x)))) {
        ## return Date if resolution == D
        return(as.Date(x))
    }

    ## Check for timezone
    if (any(grepl("+", x, fixed=TRUE))) {
        ## First, strip out the : in the time zone
        x <- sub("^(.*[+-][0-9]{2}):([0-9]{2})$", "\\1\\2", x)
        pattern <- "%Y-%m-%dT%H:%M:%OS%z"
    } else {
        pattern <- "%Y-%m-%dT%H:%M:%OS"
    }
    ## Then parse
    return(strptime(x, pattern, tz="UTC"))
}
