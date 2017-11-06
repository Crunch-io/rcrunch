#' Append one Crunch dataset to another
#'
#' @param dataset1 a CrunchDataset
#' @param dataset2 another CrunchDataset, or possibly a data.frame. If
#' `dataset2` is not a Crunch dataset, it will be uploaded as a new
#' dataset before appending. If it is a CrunchDataset, it may be subsetted with
#' a filter expression on the rows and a selection of variables on the columns.
#' @param autorollback Deprecated. This option no longer does anything.
#' @return `dataset1`, updated with `dataset2`, potentially filtered on rows and
#' variables, appended to it.
#' @export
appendDataset <- function (dataset1, dataset2, autorollback) {
    stopifnot(is.dataset(dataset1))

    if (!missing(autorollback)) {
        warning("The ", sQuote("autorollback"),
                " argument is deprecated and has no effect", call.=FALSE)
    }

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

    ## Assemble the payload
    payload <- list(dataset=self(dataset2))
    ## Include a variable map, if appropriate
    payload$where <- variablesFilter(dataset2)
    ## And filter the rows, if appropriate
    payload$filter <- zcl(activeFilter(dataset2))

    ## Preventatively, delete the primary key on dataset1 so that this appends
    ## and not "upsert"
    pk(dataset1) <- NULL

    ## POST the batch. This will error with a useful message if it fails
    dataset1 <- addBatch(dataset1, body=payload)
    invisible(dataset1)
}
