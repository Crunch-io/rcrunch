parse_column <- list(
    numeric=function (col, variable, mode) {
        missings <- vapply(col, Negate(is.numeric), logical(1))
        col[missings] <- NA_real_
        return(as.numeric(unlist(col)))
    },
    text=function (col, variable, mode) {
        missings <- vapply(col, Negate(is.character), logical(1))
        col[missings] <- NA_character_
        return(as.character(unlist(col)))
    },
    categorical=function (col, variable, mode=NULL) {
        vartype <- "categorical_factor"
        ## Deal with mode. Valid modes: factor (default), numeric, id
        if (is.null(mode)) {
            ## Default from the base method is "any", which means nothing to us
            mode <- "any"
        }
        if (mode == "numeric") {
            vartype <- "categorical_numeric_values"
        } else if (mode == "id") {
            vartype <- "categorical_ids"
        } else if (type(variable) == "categorical" && is.3vl(variable)) {
            ## Temporary: restrict on type==categorical so MRs don't get
            ## turned into logicals (until we're ready to flip that switch)
            vartype <- "logical"
        }
        return(columnParser(vartype)(col, variable))
    },
    categorical_factor=function (col, variable, mode=NULL) {
        out <- columnParser("numeric")(col)
        cats <- na.omit(categories(variable))
        out <- factor(names(cats)[match(out, ids(cats))], levels=names(cats))
        return(out)
    },
    categorical_ids=function (col, variable, mode) {
        missings <- vapply(col, is.list, logical(1)) ## for the {?:values}
        col[missings] <- lapply(col[missings], function (x) x[["?"]])
        return(as.numeric(unlist(col)))
    },
    categorical_numeric_values=function (col, variable, mode) {
        out <- columnParser("numeric")(col)
        cats <- na.omit(categories(variable))
        out <- values(cats)[match(out, ids(cats))]
        return(out)
    },
    logical=function (col, variable, mode=NULL) {
        out <- columnParser("numeric")(col)
        return(as.logical(out))
    },
    categorical_array=function (col, variable, mode) {
        out <- columnParser("categorical")(unlist(col), variable, mode)
        out <- as.data.frame(matrix(out,
            ncol=length(tuple(variable)$subvariables), byrow=TRUE))
        if (namekey(variable) == "alias") {
            names(out) <- aliases(subvariables(variable))
        } else {
            names(out) <- names(subvariables(variable))
        }
        return(out)
    },
    datetime=function (col, variable, mode) {
        out <- columnParser("text")(col)
        return(from8601(out))
    },
    boolean=function (col, variable, mode) {
        missings <- vapply(col, Negate(is.logical), logical(1))
        col[missings] <- NA
        return(as.logical(unlist(col)))
    }
)
columnParser <- function (vartype) {
    if (vartype == "multiple_response") {
        vartype <- "categorical_array"
    }
    return(parse_column[[vartype]] %||% parse_column[["numeric"]])
}

## Pulled to a function so that it can be mocked in tests
.categoricalPageSize <- function () 200000L

.crunchPageSize <- function (variable) {
    ## Determine a safe page size for paginating GET values/
    categorical.size <- .categoricalPageSize()
    if (is.variable(variable)) {
        vartype <- type(variable)
        if (is.Array(variable)) {
            ## It's effectively categorical, so let's just divide by number
            ## of subvars
            return(categorical.size %/% length(subvariables(tuple(variable))))
        } else if (vartype == "categorical") {
            return(categorical.size)
        } else if (vartype %in% c("numeric", "datetime")) {
            return(categorical.size %/% 2L)
        } else if (vartype == "text") {
            ## Throttle aggressively
            return(categorical.size %/% 40L)
        }
    } else {
        ## Crunch Expression. Probably fine, but let's throttle a bit to be safe
        return(categorical.size %/% 4L)
    }
}

getValues <- function (x, ...) {
    paginatedGET(shojiURL(x, "views", "values"), list(...),
        limit=.crunchPageSize(x))
}

paginatedGET <- function (url, query, offset=0, limit=1000, table=FALSE) {
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
#' Crunch Variables reside on the server, allowing you to work with
#' datasets that are too big to bring into memory on your machine. Many
#' functions, such as `max`, `mean`, and [crtabs()], translate your commands
#' into API queries and return only the result. But, not every operation you'll
#' want to perform has been implemented on the Crunch servers. If you need to do
#' something beyond what is currently supported, you can bring a variable's
#' data into R with `as.vector(ds$var)` and work with it like any
#' other R vector.
#'
#' `as.vector` transfers data from Crunch to a local R session. Note:
#' `as.vector` returns the vector in the row order of the dataset. If filters
#' are set that specify an order that is different from the row order of the
#' dataset, the results will ignore that order. If you need the vector ordered
#' in that way, use syntax like `as.vector(ds$var)[c(10, 5, 2)]` instead.
#'
#' @param x a CrunchVariable
#' @param mode for Categorical variables, one of either "factor" (default,
#' which returns the values as factor); "numeric" (which returns the numeric
#' values); or "id" (which returns the category ids). If "id", values
#' corresponding to missing categories will return as the underlying integer
#' codes; i.e., the R representation will not have any `NA` elements. Otherwise,
#' missing categories will all be returned `NA`. For non-Categorical
#' variables, the `mode` argument is ignored.
#' @return an R vector of the type corresponding to the Variable. E.g.
#' CategoricalVariable yields type factor by default, NumericVariable yields
#' numeric, etc.
#' @seealso [as.data.frame][as.data.frame.CrunchDataset] for another interface
#' for (lazily) fetching data from the server as needed; [exportDataset()] for
#' pulling all of the data from a dataset.
#' @name variable-to-R
NULL

#' @rdname variable-to-R
#' @export
setMethod("as.vector", "CrunchVariable", function (x, mode) {
    f <- zcl(activeFilter(x))
    # TODO: this will return in dataset order even if there is a filter is
    # specified that is not in the same order as rows
    # (eg as.vector(ds$v1[c(10:1)])) as.vector should re-order by default
    # see CrunchDataFrame for one way this could be accomplished
    columnParser(type(x))(getValues(x, filter=toJSON(f)), x, mode)
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
