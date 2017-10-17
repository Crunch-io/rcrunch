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
#' Merging a CrunchDataFrame with a local dataframe is experimental and might
#' result in unexpected results. One known issue is that using `merge` on a
#' CrunchDataFrame will change the both the CrunchDataFrame used as input as
#' well as create a new CrunchDataFrame.
#'
#' @param x a CrunchDataFrame
#' @param y a standard data.frame
#' @param by name of the variable to match in both data sources (default: the 
#' intersection of the names of x and y)
#' @param by.x name of the variable to match in x
#' @param by.y name of the variable to match in y
#' @param sort character, either "x" or "y" (default: "x"). Which of the inputs 
#' should be used for the output order. Unlike merge.data.frame, 
#' `merge.CrunchDataFrame` will not re-sort the order of the output. It will use
#' the order of either `x` or `y`.
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
                " are not currently supported by merge.CrunchDataFrame. The ", 
                "results will include all rows from whichever argument (x or y) ",
                "is used to sort.")
    }
    
    # TODO: find a better way to subset the columns needed
    x_index <- as.data.frame(x[[by.x]])
    x_index$x_index <- as.numeric(row.names(x_index))
    colnames(x_index) <- c(by.x, "x_index")
    
    y_index <- as.data.frame(y[[by.y]])
    y_index$y_index <- as.numeric(row.names(y_index))
    colnames(y_index) <- c(by.y, "y_index")
    
    new_cols_map <- merge(x_index, y_index, by.x=by.x, by.y=by.y,
                          all=TRUE, sort=FALSE)
    
    # Re-create the crunchdataset. 
    # TODO: make sure to check for local variables!
    old_vars <- names(x)
    new_x <- as.data.frame(attr(x, "crunchDataset"))
    old_local_vars <- setdiff(old_vars, names(attr(x, "crunchDataset")))
    if (length(old_local_vars) > 0) {
        old_local_df <- x[,old_local_vars, drop=FALSE]
    } else {
        old_local_df <- NULL
    }
    
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
    
    # TODO: find a way to reorder the columns without touching them
    # add back in the old local variables
    if (!is.null(old_local_df)) {
        ord <- attr(new_x, "order") %||% seq_len(nrow(new_x))
        new_x[,old_local_vars] <- old_local_df[ord,]
    }
    
    new_cols <- y[new_cols_map$y_index,]
    for (col in colnames(new_cols)) {
        if (!col %in% by.x) {
            # only assign new columns
            # TODO: check names, do something intelligent if they are already there.
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