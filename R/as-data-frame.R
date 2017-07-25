CrunchDataFrame <- function (dataset, order) {
    ## S3 constructor method for CrunchDataFrame. terms.formula doesn't seem
    ## to like S4 subclass of environment
    stopifnot(is.dataset(dataset))
    out <- new.env()
    out$.crunchDataset <- dataset
    out$.names <- c()
    # set the order of the dataset based on order function, if no order is 
    # given return all rows in the dataset in the order they appear
    if (is.null(order)) {
        out$.n_rows <- nrow(dataset)
        out$.order <- order
    } else {
        out$.order <- order
        out$.n_rows <- length(order)
    }
    
    with(out, {
        ## Note the difference from as.environment: wrapped in as.vector
        for (.a in aliases(allVariables(dataset))) {
            eval(substitute(delayedAssign(v, {
                if (is.null(.order)) {
                    as.vector(.crunchDataset[[v]])
                } else {
                    # as.numeric(factor(.order)) gets the dense ranking of the 
                    # ordering so that the non-duplicated results returned in 
                    # crunch order can be used in the order specified in .order.
                    as.vector(.crunchDataset[[v]][.order])[as.numeric(factor(.order))]
                }
                }),
                list(v=.a)))
            .names <- c(.names, .a)
        }
    })
    
    class(out) <- "CrunchDataFrame"
    return(out)
}

setOldClass("CrunchDataFrame")

#' @export
dim.CrunchDataFrame <- function (x) {
    return(c(x$.n_rows, length(ls(x))))
}

#' @export
names.CrunchDataFrame <- function (x) x$.names

#' as.data.frame method for CrunchDataset
#'
#' This method is defined principally so that you can use a CrunchDataset as
#' a `data` argument to other R functions (such as
#' `\link[stats]{lm}`). Unless you give it the `force==TRUE`
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
#' @param order vector of indeces. The order that the rows of the dataset should be presented as (default: `NULL`). If `NULL`, then the Crunch Dataset order will be used.
#' @param ... additional arguments passed to as.data.frame.default
#' @return an object of class `CrunchDataFrame` unless `force`, in
#' which case the return is a `data.frame`.
#' @name dataset-to-R
NULL

#' @rdname dataset-to-R
#' @export
as.data.frame.CrunchDataset <- function (x, row.names = NULL, optional = FALSE,
                                        force=FALSE, order = NULL, ...) {
    out <- CrunchDataFrame(x, order = order)
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
#' @param x a CrunchDataFrame
#' @param y a standard data.frame
#' @param by.x name of the variable to match
#' @param by.y name of the variable to match
#' @param sort character, either "x" or "y" (default: "x"). Which of the inputs should be used for the output order. Unlike merge.data.frame, merge.CrunchDataFrame will not re-sort the order of the output. It will use the order of either x or y. 
#' @param ... ignored for now
#' 
#' @export
merge.CrunchDataFrame  <- function (x, y, by.x, by.y, sort = "x", ...) {
    if (missing(by.x) | missing(by.y)) {
        halt("Must supply both a by.x and a by.y to match by.")
    }
    if (!sort %in% c("x", "y")) {
        halt("The sort argument must be either ", dQuote("x"), " or ", 
             dQuote("y"), ". Got ", dQuote(substitute(sort)), " instead.")
    }
    
    # TODO: find a better way to subset the columns needed
    row_index <- as.data.frame(x[[by.x]])
    colnames(row_index) <- by.x
    
    # Duplicate the enviornment (so we are not manipulating in place)
    new_x <- new.env()
    for(n in ls(x, all.names=TRUE)) assign(n, get(n, x), new_x)

    new_cols <- merge(row_index, y, all.x=TRUE)

    
    for (col in colnames(new_cols)) {
        if (!col %in% by.x) {
            if (length(new_cols[,col]) != nrow(x)) {
                halt("The number of rows in x (", nrow(x), ") and y (", length(new_cols[,col]),
                     ") must be the same.")
            }
            # only assign new columns
            # todo: check names, do something intelligent if theey are already there.
            assign(col, new_cols[,col], envir = new_x)
            new_x$.names <- c(new_x$.names, col)   
        }
    }
    
    class(new_x) <- "CrunchDataFrame"
    return(new_x)
}