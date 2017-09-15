#' CrunchDataFrame
#'
#' CrunchDataFrames are designed to mimic the ways that [`data.frame`]s are used. They should be a drop-in replacement in many plaes where data.frames are used.
#' 
#'  CrunchDataFrames are generated not by downloading all of the variables from a dataset, but rather only the variables that are needed by subsequent functions. So, if you create a CrunchDataFrame, and then run a linear model using `lm()`, only the variables used by the linear model will be downloaded.
#'  
#' CrunchDataFrames can be altered (that is: adding more columns, removing columns, subsetting rows, etc.) with the same `[`, `[[`, and `$` syntax as data.frames.
#'
#' @param x a CrunchDataFrame
#' @param i indicators for rows (integers or logicals)
#' @param j indicators for columns (names, integers, or logicals)
#' @param value value to place (or replace) in the CrunchDataFrame
#' @param drop logical. If `TRUE` the result is coerced to the lowest possible dimension. The default is to drop if only one column is left, but not to drop if only one row is left.
#'
#' @name CrunchDataFrame
NULL

CrunchDataFrame <- function (dataset, row.order = NULL, categorical.mode = "factor", include.hidden = FALSE) {
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
                get_var_from_server(v, ds, mode)
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
    j <- grab_col_names(x, j)
    row_inds <- grab_row_ind(x, i)
    
    # grab the columns
    out <- lapply(j, function(col) {
        col_data <- x[[col]]
        if (is.data.frame(col_data)) {
            return(col_data[row_inds,])
        } else {
            return(col_data[row_inds])
        }
    })
    
    # set names, if j is a character use that, if not grab names from the CDF
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
    j <- grab_col_names(x, j)
    row_inds <- grab_row_ind(x, i)
    
    # TODO: check value length
    
    # split value string by the length of row indicators
    values <- split(value, ceiling(seq_along(value)/length(row_inds)))
    for (i in seq_along(j)) {
        col_name <- j[i]
        x <- set_CDF_var(col_name = col_name, row_inds = row_inds,
                         cdf = x, value = values[[i]])
    }
    return(x)
}

grab_col_names <- function (x, j) {
    if (missing(j)) {
        # if there's no j, grab all columns
        j <- names(x)
    } else if (is.logical(j)) {
        if (length(j) != ncol(x)) {
            halt("when using a logical to subset columns, the logical vector ",
                 "must have the same length as the number of columns.")
        }
        j <- names(x)[j]
    }
    return(j)
}

grab_row_ind <- function (x, i) {
    if (missing(i)) {
        # if there is no row specification, use all rows.
        row_inds <- seq_len(nrow(x))
    } else {
        if (is.logical(i)) {
            if (length(i) != nrow(x)) {
                halt("when using a logical to subset rows, the logical vector ",
                     "must have the same length as the number of rows")
            }
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
    col_name <- col_name_from_index(x, i)
    
    return(get_CDF_var(col_name, x, mode = attr(x, "mode")))
}

#' @export
#' @rdname CrunchDataFrame
`[[<-.CrunchDataFrame` <- function (x, i, value) {
    col_name <- col_name_from_index(x, i)
    
    x <- set_CDF_var(col_name = col_name, cdf = x, value = value)
    return(x)
}

col_name_from_index <- function(x, i) {
    if (is.character(i)) {
        col_name <- i
        # check if any don't exist
    } else if (is.numeric(i)) {
        col_name <- names(x)[i]
    } else {
        halt("column subsetting must be done with either numeric or ",
             "a character")
    }
    return(col_name)
}

#' @export
#' @rdname CrunchDataFrame
`$.CrunchDataFrame` <- function (x, i) {
    return(get_CDF_var(i, x, mode = attr(x, "mode")))
}

#' @export
#' @rdname CrunchDataFrame
`$<-.CrunchDataFrame` <- function (x, i, value) {
    x <- set_CDF_var(col_name = i, cdf = x, value = value)
    return(x)
}

get_CDF_var <- function (col_name, cdf, ...) {
    if (!is(cdf, "CrunchDataFrame")) { 
        halt("The cdf argument must be a CrunchDataFrame, got ", class(cdf), " instead.")
    }
    if (!col_name %in% names(cdf)) {
        halt("The variable ", dQuote(col_name), " is not found in the CrunchDataFrame.")
    }
    
    return(get(col_name, cdf)) # need [ord] here?
}

get_var_from_server <- function (col_name, cdf, ...) {
    ord <- attr(cdf, "order")
    ds <- attr(cdf, "crunchDataset")
    if (!is.null(ord)) {
        return(
            as.vector(ds[[col_name]][ord], ...)[match(ord, sort(unique(ord)))]
        )
    } else {
        # no reordering if order is null
        return(as.vector(ds[[col_name]], ...))
    }
}

set_CDF_var <- function (col_name, row_inds, cdf, value) {
    if (!is(cdf, "CrunchDataFrame")) { 
        halt("The cdf argument must be a CrunchDataFrame, got ", class(cdf), " instead.")
    }
    
    # remove the variable if the value is NULL
    if (is.null(value) & missing(row_inds)) {
        return(invisible(rm_CDF_var(col_name = col_name, cdf = cdf)))
    }
    
    if (missing(row_inds)) {
        row_inds <- seq_len(nrow(cdf))
    }
    
    # validate row lengths
    if (length(value) == 1 & length(row_inds) > 1) {
        # if there is a single value, but more than one row, then we need to 
        # repeat the value for as many times as there are rows. This is the 
        # only exception to the rule that there must be the same number of 
        # values as rows.
        value <- rep(value, length(row_inds))
    } else if (is.data.frame(value)) {
        # we got a data.frame as values, likelythis is an array or MR varaible
        # unclass does what data.frame(v1=data.frame(a=1, b=2)) would do
        # value <- unlist(value, recursive = FALSE, use.names = FALSE)
        # concat names
        # vnames <- unlist(vnames[ncols > 0L])
        # set multiple variables
        value <- I(value)
    } else if (length(value) != length(row_inds)) {
        halt("replacement has ", length(value),
             " rows, the CrunchDataFrame has ", nrow(cdf))
    }
    
    # if we are replacing some rows, fill in with NAs, or grab old vector
    if (length(row_inds) != nrow(cdf)) {
        if (!col_name %in% names(cdf)) {
            empty_vector <- rep(NA, nrow(cdf))
            empty_vector[row_inds] <- value
            value <- empty_vector
        } else {
            old_vector <- get(col_name, cdf)
            old_vector[row_inds] <- value
            value <- old_vector 
        }
    }
    
    # add values
    assign(col_name, value, cdf)
    
    # add to names
    attr(cdf, "col_names") <- c(names(cdf), col_name)
    
    return(invisible(cdf))
}

rm_CDF_var <- function (col_name, cdf) {
    if (!is(cdf, "CrunchDataFrame")) { 
        halt("The cdf argument must be a CrunchDataFrame, got ", class(cdf), " instead.")
    }
    
    # remove column
    rm(list = col_name, envir = cdf)
    
    # remove from names
    attr(cdf, "col_names") <- setdiff(names(cdf), col_name)
    
    return(invisible(cdf))
}