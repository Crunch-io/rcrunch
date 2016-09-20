#' Append one Crunch dataset to another
#'
#' @param dataset1 a CrunchDataset
#' @param dataset2 another CrunchDataset, or possibly a data.frame. If
#' \code{dataset2} is not a Crunch dataset, it will be uploaded as a new
#' dataset before appending.
#' @param autorollback logical: If the append fails, revert the dataset back
#' to its state before attempting to append? Default is \code{TRUE}, and you
#' probably won't want to change that.
#' @param cleanup Deprecated in favor of \code{autorollback}. See also
#' \code{\link{cleanseBatches}}.
#' @return A CrunchDataset with \code{dataset2} appended to \code{dataset1}
#' @export
appendDataset <- function (dataset1, dataset2, autorollback=TRUE, cleanup=autorollback) {
    if (!missing(cleanup)) {
        warning('Argument "cleanup" is deprecated. Use "autorollback" instead.',
            call.=FALSE)
    }
    stopifnot(is.dataset(dataset1))
    if (!is.dataset(dataset2)) {
        temp.ds.name <- paste("Appending to", name(dataset1), now())
        message("Creating ", dQuote(temp.ds.name), " as temporary dataset")
        dataset2 <- newDataset(dataset2, name=temp.ds.name)
        ## TODO: on exit, delete dataset2
    }

    ## Validate
    if (identical(self(dataset1), self(dataset2))) {
        halt("Cannot append dataset to itself")
    }

    ## POST the batch. This will error with a useful message if it fails
    dataset1 <- addBatch(dataset1, dataset=self(dataset2),
        autorollback=autorollback)
    invisible(dataset1)
}
