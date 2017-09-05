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
    attr(out, "crunchVars") <- var_names
    attr(out, "mode") <- categorical.mode
    
    with(out, {
        ## Note the difference from as.environment: wrapped in as.vector
        for (.a in var_names) {
            eval(substitute(delayedAssign(v, {
                get_CDF_var(v, ds, mode)
            }),
            list(v=.a, ds=out, mode = attr(out, "mode"))))
        }
    })
    
    # set the order of the dataset based on row.order, if no row.order is 
    # given return all rows in the dataset in the order they appear
    attr(out, "order") <- row.order
    
    class(out) <- "CrunchDataFrame"
    return(out)
}

setOldClass("CrunchDataFrame")

#' @export
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
names.CrunchDataFrame <- function (x) attr(x, "col_names")

#' as.data.frame method for CrunchDataset
#'
#' This method is defined principally so that you can use a CrunchDataset as
#' a `data` argument to other R functions (such as
#' `[stats::lm]`). Unless you give it the `force==TRUE`
#' argument, this function does not in fact return a `data.frame`: it
#' returns an object with an interface like a data.frame, such that you get
#' R vectors when you access its columns (unlike a CrunchDataset, which
#' returns CrunchVariable objects). This allows modeling functions that
#' require select columns of a dataset to retrieve only those variables from
#' the remote server, rather than pulling the entire dataset into local
#' memory.
#'
#' @param x a CrunchDataset
#' @param row.names part of as.data.frame signature. Ignored.
#' @param optional part of as.data.frame signature. Ignored.
#' @param force logical: actually coerce the dataset to `data.frame`, or
#' leave the columns as unevaluated promises. Default is `FALSE`.
#' @param row.order vector of indeces. Which, and their order, of the rows of 
#'  the dataset should be presented as (default: `NULL`). If `NULL`, then the 
#'  Crunch Dataset order will be used.
#' @param categorical.mode what mode should categoricals be pulled as? One of 
#' factor, numeric, id (default: factor)
#' @param include.hidden should hidden variables be included? (default: `FALSE`)
#' @param ... additional arguments passed to as.data.frame.default
#' @return an object of class `CrunchDataFrame` unless `force`, in
#' which case the return is a `data.frame`.
#' @name dataset-to-R
NULL

#' @rdname dataset-to-R
#' @export
as.data.frame.CrunchDataset <- function (x, row.names = NULL, optional = FALSE,
                                        force=FALSE, categorical.mode = "factor",
                                        include.hidden = FALSE,
                                        row.order = NULL, ...) {
    out <- CrunchDataFrame(x, row.order = row.order,
                           categorical.mode = categorical.mode,
                           include.hidden = include.hidden)
    if (force) {
        out <- as.data.frame(out)
    }
    return(out)
}

#' @rdname dataset-to-R
#' @export
as.data.frame.CrunchDataFrame <- function (x, row.names = NULL, optional = FALSE, ...) {
    ds <- attr(x, "crunchDataset")
    default.stringsAsFactors <- function () FALSE
    limit <- min(c(10000, getOption("crunch.data.frame.limit")))
    if (nrow(ds) * ncol(ds) > limit) {
        ## TODO: switch to downloading CSV and reading that?
        halt("Dataset too large to coerce to data.frame. ",
            "Consider subsetting it first")
    }
    crunch_var_names <- attr(x, "crunchVars")
    # todo: something intelligent with modes
    out <- lapply(crunch_var_names, function(var) as.vector(ds[[var]]))
    names(out) <- crunch_var_names
    
    col_names <- attr(x, "col_names")
    local_var_names <- col_names[!col_names %in% crunch_var_names]
    if (length(local_var_names) > 0) {
        out <- cbind(out, x[,local_var_names, drop = FALSE])
    }
    return(structure(out, class="data.frame", row.names=c(NA, -nrow(ds))))
}


#' Merge a CrunchDataFrame
#'
#' `merge`ing a CrunchDataFrame with a local dataframe is useful in situations
#' where you have new information in your local R session that you want to
#' connect with Crunch data. For example, for making
#' plots with Crunch and non-Crunch data. It produces a hybrid CrunchDataFrame
#' that has the local data attached to it, but like normal CrunchDataFrames
#' it is still judicious about downloading data from the server only when it
#' is needed.
#'
#' Merging a CrunchDataFrame with a local dataframe does not allow specifying
#' all rows from both sources. Instead, the resulting CrunchDataFrame will
#' include all of the rows in whichever source is used for sorting (x or y). So
#' if you specify `sort="x"` (the default) all rows of x will be present but
#' rows in y that do not match with rows in x will not be present.
#'
#' Merging a CrunchDataFrame with a local dataframe is experiemental and might
#' result in unexpected results. One known issue is that using `merge` on a
#' CrunchDataFrame will change the both the CrunchDataFrame used as input as
#' well as create a new CrunchDataFrame.
#'
#' @param x a CrunchDataFrame
#' @param y a standard data.frame
#' @param by name of the variable to match in both data sources (default: the intersection of the names of x and y)
#' @param by.x name of the variable to match in x
#' @param by.y name of the variable to match in y
#' @param sort character, either "x" or "y" (default: "x"). Which of the inputs should be used for the output order. Unlike merge.data.frame, merge.CrunchDataFrame will not re-sort the order of the output. It will use the order of either `x` or `y`.
#' @param ... ignored
#' @name merge
#'
#' @return a CrunchDataFrame with columns from both `x` and `y`
#'
#' @export
merge.CrunchDataFrame  <- function (x, y, by=intersect(names(x), names(y)),
                                    by.x=by, by.y=by, sort = c("x", "y"), ...) {
    by.x <- fix_bys(x, by.x)
    by.y <- fix_bys(y, by.y)
    sort <- match.arg(sort)

    # TODO: instead of not allowing use of `all`, allow it and do the right thing.
    if (any(startsWith(ls(list(...)), "all"))) {
        warning("options ", serialPaste(dQuote(c("all", "all.x", "all.y"))),
                " are not currently supported by merge.CrunchDataFrame. The results will include all rows from whichever argument (x or y) is used to sort.")
    }

    # Duplicate the enviornment (so we are not manipulating in place)
    # using `for(n in ls(x, all.names=TRUE)) assign(n, get(n, x), new_x)`
    # will evaluate each column, which is not actually what we want
    # pryr:::promise_code (which can be compiled from promise.cpp separately)
    # includes a way to extract the promise code, which works, but should only
    # be used on actual promises, so would need some safe-guarding.
    # instead we are copying the enviornment which modifies the CrunchDataFrame
    # that was given in place.
    new_x <- x

    # TODO: find a better way to subset the columns needed
    x_index <- as.data.frame(x[[by.x]])
    x_index$x_index <- as.numeric(row.names(x_index))
    colnames(x_index) <- c(by.x, "x_index")

    y_index <- as.data.frame(y[[by.y]])
    y_index$y_index <- as.numeric(row.names(y_index))
    colnames(y_index) <- c(by.y, "y_index")

    new_cols_map <- merge(x_index, y_index, by.x=by.x, by.y=by.y, all=TRUE, sort=FALSE)

    # TODO: split out into separate functions that deal with the
    # CrunchDataFrame and the data.frame independently, and then allow for
    # either to be x or y.
    if (sort == "x") {
        new_cols_map <- new_cols_map[with(new_cols_map, order(x_index, y_index)), ]
        if (!identical(new_cols_map$x_index, x_index$x_index)) {
            # add ordering if theres more than one y for each x
            attr(new_x, "order") <- new_cols_map$x_index
            # assign(by.x, x[[by.x]][attr(x, "order")], new_x) # not needed?
        }
        # remove NAs from x_index?
    } else if (sort == "y") {
        new_cols_map <- new_cols_map[with(new_cols_map, order(y_index, x_index)), ]
        new_cols_map <- new_cols_map[!is.na(new_cols_map$y_index),]
        attr(new_x, "order") <- new_cols_map$x_index
        # need to remap the by.x columns because new_x was evaluated already
        # assign(by.x, x[[by.x]][attr(new_x, "order")], new_x) # not needed?
    }

    new_cols <- y[new_cols_map$y_index,]
    for (col in colnames(new_cols)) {
        if (!col %in% by.x) {
            # only assign new columns
            # todo: check names, do something intelligent if they are already there.
            assign(col, new_cols[,col], envir = new_x)
            attr(new_x, "col_names") <- c(names(new_x), col)
        }
    }

    return(new_x)
}

fix_bys <- function (data, by) {
    ## Do validations and return a proper, legal "by" variable, if possible
    if (!is.data.frame(data) & all(!class(data) %in% "CrunchDataFrame")) {
        halt(substitute(data), " must be a data.frame or CrunchDataFrame")
    }
    if (is.character(by)) {
        if (length(by) != 1) {
            halt("by must reference one and only one variable")
        }
        if (!by %in% ls(data)) {
            halt(by, " does not reference a variable in ", substitute(data))
        }
    }
    return(by)
}

#' @export
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
        # if there's no j, grab all rows
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
`[[.CrunchDataFrame` <- function (x, i) {
    col_name <- col_name_from_index(x, i)
    
    return(get_CDF_var(col_name, x, mode = attr(x, "mode")))
}

#' @export
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
`$.CrunchDataFrame` <- function (x, i) {
    return(get_CDF_var(i, x, mode = attr(x, "mode")))
}

#' @export
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
    
    if (col_name %in% attr(cdf, "crunchVars")) {
        ord <- attr(cdf, "order")
        ds <- attr(cdf, "crunchDataset")
        if (!is.null(ord)) {
            return(as.vector(ds[[col_name]][ord], ...)[match(ord, sort(unique(ord)))])
        } else {
            # no reordering if order is null
            return(as.vector(ds[[col_name]], ...))
        }
    } else {
        return(get(col_name, cdf)) # need [ord] here?
    }
}

set_CDF_var <- function (col_name, row_inds, cdf, value) {
    if (!is(cdf, "CrunchDataFrame")) { 
        halt("The cdf argument must be a CrunchDataFrame, got ", class(cdf), " instead.")
    }
    
    if (col_name %in% attr(cdf, "crunchVars")) {
        halt("Cannot over-write data from a Crunch variable.")
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
