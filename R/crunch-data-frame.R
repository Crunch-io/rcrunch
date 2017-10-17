#' CrunchDataFrame
#'
#' CrunchDataFrames are designed to mimic the ways that [`data.frame`]s are used.
#' They should be a drop-in replacement in many places where data.frames are used.
#' 
#'  CrunchDataFrames are generated not by downloading all of the variables from 
#'  a dataset, but rather only the variables that are needed by subsequent 
#'  functions. So, if you create a CrunchDataFrame, and then run a linear model 
#'  using `lm()`, only the variables used by the linear model will be downloaded.
#'  
#' CrunchDataFrames can be altered (that is: adding more columns, removing 
#' columns, subsetting rows, etc.) with the same `[`, `[[`, and `$` syntax as 
#' data.frames.
#'
#' @param x a CrunchDataFrame
#' @param i indicators for rows (integers or logicals)
#' @param j indicators for columns (names, integers, or logicals)
#' @param value value to place (or replace) in the CrunchDataFrame
#' @param drop logical. If `TRUE` the result is coerced to the lowest possible 
#' dimension. The default is to drop if only one column is left, but not to drop
#' if only one row is left.
#'
#' @name CrunchDataFrame
NULL

CrunchDataFrame <- function (dataset, row.order = NULL,
                             categorical.mode = "factor",
                             include.hidden = FALSE) {
    ## S3 constructor method for CrunchDataFrame. terms.formula doesn't seem
    ## to like S4 subclass of environment
    stopifnot(is.dataset(dataset))
    
    if (include.hidden) {
        var_names <- aliases(allVariables(dataset))
    } else {
        var_names <- aliases(variables(dataset))
    }
    
    out <- new.env()
    attr(out, "crunchDataset") <- dataset
    attr(out, "col_names") <- var_names
    attr(out, "mode") <- categorical.mode
    
    with(out, {
        ## Note the difference from as.environment: wrapped in as.vector
        for (.a in var_names) {
            eval(substitute(delayedAssign(v, {
                getVarFromServer(col_name = v, crdf = ds, mode = mode)
            }),
            list(v=.a, ds=out, mode = attr(out, "mode"))))
        }
    })
    
    # set the order of the dataset based on row.order, if no row.order is 
    # given return all rows in the dataset in the order they appear
    attr(out, "order") <- row.order
    
    # TODO: add "data.frame" here too
    class(out) <- "CrunchDataFrame"
    return(out)
}

setOldClass("CrunchDataFrame")

#' @export
#' @rdname CrunchDataFrame
dim.CrunchDataFrame <- function (x) {
    if (is.null(attr(x, "order"))) {
        # if there is no ordering, the num of rows is the same as dataset
        n_rows <- nrow(attr(x, "crunchDataset"))
    } else {
        # if there is an ordering, the num of rows is the length of the ordering
        n_rows <- length(attr(x, "order"))
    }
    return(c(n_rows, length(ls(x))))
}

#' @export
#' @rdname CrunchDataFrame
names.CrunchDataFrame <- function (x) attr(x, "col_names")



#' @export
#' @rdname CrunchDataFrame
`[.CrunchDataFrame` <- function (x, i, j, drop = TRUE) {
    j <- grabColNames(x, j)
    row_inds <- grabRowInd(x, i)
    
    # grab the columns
    out <- lapply(j, function(col) {
        col_data <- x[[col]]
        if (is.data.frame(col_data)) {
            return(col_data[row_inds,])
        } else {
            return(col_data[row_inds])
        }
    })
    
    # set names, if j is a character use that, if not grab names from the crdf
    if (is.character(j)) {
        names(out) <- j
    } else {
        names(out) <- names(x)[j]
    }
    
    if (length(out) == 1 & drop) {
        # single column, so return vector
        out <- out[[1]]
    } else {
        out <- structure(out, class="data.frame", row.names=seq_along(row_inds))
    }
    
    return(out)
}

#' @export
#' @rdname CrunchDataFrame
`[<-.CrunchDataFrame` <- function (x, i, j, value) {
    j <- grabColNames(x, j)
    row_inds <- grabRowInd(x, i)
    
    # check value length, repeat if necessary
    cells_to_fill <- length(row_inds) * length(j)
    if (length(value) != cells_to_fill && cells_to_fill %% length(value) == 0) {
        # if we can cleanly recycle values to get the number of cells, do so
        value <- rep_len(value, cells_to_fill)
    }
    if (length(value) != cells_to_fill) {
        # the number of values doesn't align with the number of cells to fill
        halt("replacement has ", length(value), " items, need ", cells_to_fill)
    }
    
    # split value into chunks, one for each column to replace into.
    # this allows for the same behavior with (odd) insertions like:
    # foo <- data.frame(foo=c(1, 2, 3, 4), bar=c(5, 6, 7, 8),
    #                   baz = c(1, 2, 3, 4), qux = c(5, 6, 7, 8))
    # foo[c(1, 2), c(3, 4)] <- c(10, 20, 30, 40)
    values <- split(value, ceiling(seq_along(value)/length(row_inds)))
    for (i in seq_along(j)) {
        col_name <- j[i]
        x <- setCrdfVar(col_name = col_name, row_inds = row_inds,
                         crdf = x, value = values[[i]])
    }
    return(x)
}

#' grab column names for subsetting crdfs
#' 
#' Take a character, logical, or integer vector and return the appropriate names
#' of the crdf to subset. Disallow logicals (in the case of `[[` subsetting, to
#' conform with `data.frame` methods)
#' 
#' @param x crdf to grab columns from
#' @param j character, logical, or integer vector specifying the columns to grab
#' @param allow_logical logical should logical subsetting be allowed? (default: TRUE)
#' 
#' @return character names of the columns of the crdf to select
#' @keywords internal
grabColNames <- function (x, j, allow_logical = TRUE) {
    if (missing(j)) {
        # if there's no j, grab all columns
        j <- names(x)
    } else if (is.numeric(j) | is.logical(j)) {
        j <- names(x)[j]
    } else if (!is.character(j)) {
        halt("column subsetting must be done with a numeric, character, or ",
             "logical.")
    }
    return(j)
}

#' grab row indicators for subsetting crdfs
#' 
#' @param x crdf to grab rows from
#' @param i logical or integer vector specifying the rows to grab
#' 
#' @return integers of the rows of the crdf to select
#' @keywords internal
grabRowInd <- function (x, i) {
    if (missing(i)) {
        # if there is no row specification, use all rows.
        row_inds <- seq_len(nrow(x))
    } else {
        if (is.logical(i)) {
            # recycle logicals
            i <- rep(i, ceiling(nrow(x)/length(i)))[seq_len(nrow(x))]
        } else if (!is.numeric(i)) {
            halt("row subsetting must be done with either numeric or a logical.")
        }
        row_inds <- i
    }
    return(row_inds)
}

#' @export
#' @rdname CrunchDataFrame
`[[.CrunchDataFrame` <- function (x, i) {
    return(getCrdfVar(grabColNames(x, i, allow_logical = FALSE),
                      x, mode = attr(x, "mode")))
}

#' @export
#' @rdname CrunchDataFrame
`[[<-.CrunchDataFrame` <- function (x, i, value) {
    return(setCrdfVar(col_name = grabColNames(x, i, allow_logical = FALSE),
                      crdf = x, value = value))
}

#' @export
#' @rdname CrunchDataFrame
`$.CrunchDataFrame` <- function (x, i) {
    return(getCrdfVar(i, x, mode = attr(x, "mode")))
}

#' @export
#' @rdname CrunchDataFrame
`$<-.CrunchDataFrame` <- function (x, i, value) {
    return(setCrdfVar(col_name = i, crdf = x, value = value))
}

getCrdfVar <- function (col_name, crdf, ...) {
    if (!col_name %in% names(crdf)) {
        halt("The variable ", dQuote(col_name), " is not found in the CrunchDataFrame.")
    }
    
    return(get(col_name, crdf)) # need [ord] here?
}

getVarFromServer <- function (col_name, crdf, ...) {
    ord <- attr(crdf, "order")
    ds <- attr(crdf, "crunchDataset")
    
    col_ref <- ds[[col_name]]

    if (!is.null(ord)) {
        return(as.vector(col_ref[ord], ...)[match(ord, sort(unique(ord)))])
    } else {
        # no reordering if order is null
        return(as.vector(col_ref, ...))
    }
}

setCrdfVar <- function (col_name, row_inds, crdf, value) {
    # remove the variable if the value is NULL
    if (is.null(value) & missing(row_inds)) {
        return(invisible(rmCrdfVar(col_name = col_name, crdf = crdf)))
    }
    
    if (missing(row_inds)) {
        row_inds <- seq_len(nrow(crdf))
    }
    
    # validate row lengths
    if (length(value) == 1 & length(row_inds) > 1) {
        # if there is a single value, but more than one row, then we need to 
        # repeat the value for as many times as there are rows. This is the 
        # only exception to the rule that there must be the same number of 
        # values as rows.
        value <- rep(value, length(row_inds))
    } else if (is.data.frame(value)) {
        # we got a data.frame as values, likely this is an array or MR variable
        # Flatten it so that each sub variable is its own column, without name 
        # prefixing. This is slightly different from what data.frame(v1 = 
        # data.frame(a=1, b=2)) would do.
        value <- unlist(value, recursive = FALSE)
    } else if (length(value) != length(row_inds)) {
        halt("replacement has ", length(value),
             " rows, the CrunchDataFrame has ", nrow(crdf))
    }
    
    # if we are replacing some rows, fill in with NAs, or grab old vector
    if (length(row_inds) != nrow(crdf)) {
        if (!(col_name %in% names(crdf))) {
            old_vector <- rep(NA, nrow(crdf))
        } else {
            old_vector <- get(col_name, crdf)
        }
        old_vector[row_inds] <- value
        value <- old_vector 
    }
    
    # add values
    assign(col_name, value, crdf)
    
    # add to names
    attr(crdf, "col_names") <- c(names(crdf), col_name)
    
    return(invisible(crdf))
}

rmCrdfVar <- function (col_name, crdf) {
    # remove column
    rm(list = col_name, envir = crdf)
    
    # remove from names
    attr(crdf, "col_names") <- setdiff(names(crdf), col_name)
    
    return(invisible(crdf))
}