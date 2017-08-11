CrunchDataFrame <- function (dataset, row.order = NULL) {
    ## S3 constructor method for CrunchDataFrame. terms.formula doesn't seem
    ## to like S4 subclass of environment
    stopifnot(is.dataset(dataset))
    out <- new.env()
    out$.crunchDataset <- dataset
    out$.names <- aliases(allVariables(dataset))
    # set the order of the dataset based on row.order, if no row.order is 
    # given return all rows in the dataset in the order they appear
    out$.order <- row.order
    
    with(out, {
        ## Note the difference from as.environment: wrapped in as.vector
        for (.a in out$.names) {
            eval(substitute(delayedAssign(v, {
                if (is.null(.order)) {
                    as.vector(.crunchDataset[[v]])
                } else {
                    # The API response for selected rows is in the order of the
                    # dataset, so we need to reorder it before we return it. 
                    # `match(.order, sort(unique(.order)))`` gets the dense 
                    # ranking of the ordering so that the non-duplicated 
                    # results returned in crunch order can be used in the order
                    # specified in .order.
                    as.vector(.crunchDataset[[v]][.order])[match(.order, sort(unique(.order)))]
                }
                }),
                list(v=.a)))
        }
    })
    
    class(out) <- "CrunchDataFrame"
    return(out)
}

setOldClass("CrunchDataFrame")

#' @export
dim.CrunchDataFrame <- function (x) {
    if (is.null(x$.order)) {
        # if there is no ordering, the num of rows is the same as dataset
        n_rows <- nrow(x$.crunchDataset)
    } else {
        # if there is an ordering, the num of rows is the length of the ordering
        n_rows <- length(x$.order)
    }
    return(c(n_rows, length(ls(x))))
}

#' @export
names.CrunchDataFrame <- function (x) x$.names

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
#' @param row.order vector of indeces. Which, and their order, of the rows of the dataset should be presented as (default: `NULL`). If `NULL`, then the Crunch Dataset order will be used.
#' @param ... additional arguments passed to as.data.frame.default
#' @return an object of class `CrunchDataFrame` unless `force`, in
#' which case the return is a `data.frame`.
#' @name dataset-to-R
NULL

#' @rdname dataset-to-R
#' @export
as.data.frame.CrunchDataset <- function (x, row.names = NULL, optional = FALSE,
                                        force=FALSE, row.order = NULL, ...) {
    out <- CrunchDataFrame(x, row.order = row.order)
    if (force) {
        out <- as.data.frame(out)
    }
    return(out)
}

#' @rdname dataset-to-R
#' @export
as.data.frame.CrunchDataFrame <- function (x, row.names = NULL, optional = FALSE, ...) {
    x <- x$.crunchDataset
    default.stringsAsFactors <- function () FALSE
    limit <- min(c(10000, getOption("crunch.data.frame.limit")))
    if (nrow(x) * ncol(x) > limit) {
        ## TODO: switch to downloading CSV and reading that?
        halt("Dataset too large to coerce to data.frame. ",
            "Consider subsetting it first")
    }
    out <- lapply(x, as.vector)
    names(out) <- names(x)
    return(structure(out, class="data.frame", row.names=c(NA, -nrow(x))))
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
            assign(".order", new_cols_map$x_index, new_x)
            assign(by.x, x[[by.x]][new_x$.order], new_x)
        }
        # remove NAs from x_index?
    } else if (sort == "y") {
        new_cols_map <- new_cols_map[with(new_cols_map, order(y_index, x_index)), ]
        new_cols_map <- new_cols_map[!is.na(new_cols_map$y_index),]
        assign(".order", new_cols_map$x_index, new_x)
        # need to remap the by.x columns because new_x was evaluated already
        assign(by.x, x[[by.x]][new_x$.order], new_x)
    }

    new_cols <- y[new_cols_map$y_index,]
    for (col in colnames(new_cols)) {
        if (!col %in% by.x) {
            if (length(new_cols[,col]) != nrow(new_x)) {
                halt("The number of rows in x (", nrow(new_x), 
                     ") and y (", length(new_cols[,col]), ") must be the same.")
            }
            # only assign new columns
            # todo: check names, do something intelligent if they are already there.
            assign(col, new_cols[,col], envir = new_x)
            assign(".names", c(new_x$.names, col), new_x )
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


#' as.data.frame method for VariableCatalog
#'
#' This method allows you to import the VariableCatalog into your R sessions in 
#' order to facilliate interactive use. Modifying the dataframe produced by this
#' method will not update the Crunch web app, for information about modifying 
#' variables in the app see: \code{vingette("variables",
#' package = "crunch")}  
#' 
#' @param x
#' A VariableCatalog produced by \code{variables(ds)}.
#' @param ... Additional arguments passed to \code{data.frame}
#' @param row.names part of as.data.frame signature. Ignored.
#' @param optional part of as.data.frame signature. Ignored.
#' @param keys 
#' A character vector of the variable catalog attributes which you would like 
#' included in the datafame. To include all attributes or see which ones are available
#' set keys to "all". 
#' @return A data frame which includes information about each variable stored in the variable catelog. 
#' The fields in the dataframe match the keys argument provided to the function, and each row represents a variable. 
#' @examples 
#' \dontrun{
#' ds <- loadDataset("iris")
#' vars <- variables(ds)
#' var_df <- as.data.frame(vars, keys = "all")
#' } 
#' 
#' @rdname VariableCatalog-to-Data-Frame
#' @export
as.data.frame.VariableCatalog <- function(x, 
    row.names = NULL, 
    optional = FALSE, 
    keys = c("alias", "name", "type"),
    ...) {
    if (all(keys == "all")) {
        keys <- TRUE
    }
    
    catalogToDataFrame(x, rownames = NA, keys = keys, ...)
}


