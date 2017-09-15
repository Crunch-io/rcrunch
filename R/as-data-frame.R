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
    var_names <- names(x)
    # todo: something intelligent with modes
    out <- lapply(var_names, function(var) x[[var]])
    names(out) <- var_names
    
    return(structure(out, class="data.frame", row.names=c(NA, -nrow(ds))))
}
