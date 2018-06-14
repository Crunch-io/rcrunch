#' Append one Crunch dataset to another
#'
#' With Crunch, you can add additional rows to a dataset by appending a second
#' dataset to the bottom of the original dataset. Crunch makes intelligent
#' guesses to align the variables between the two datasets and to harmonize the
#' categories and subvariables of variables, as appropriate.
#'
#' Variables are matched between datasets based on their aliases. Variables
#' present in only one of the two datasets are fine; they're handled by filling
#' in with missing values for the rows corresponding to the dataset where they
#' don't exist. For variables present in both datasets, you will have best
#' results if you ensure that the two datasets have the same variable names
#' and types, and that their categorical and array variables have consistent
#' categories. To preview how datasets will align when appended, see
#' [compareDatasets()].
#'
#' Particularly if you're appending to datasets that are already shared with
#' others, you may want to use the fork-edit-merge workflow when appending
#' datasets. This allows you to verify your changes before releasing them to
#' the other viewers of the dataset. To do this fork the dataset with
#' [forkDataset()], append the new data to the fork, ensure that the append
#' worked as expected, and then merge the fork back to the original dataset
#' with `mergeFork()`. For more, see `vignette("fork-and-merge", package
#' = "crunch")`.
#'
#' @param dataset1 a CrunchDataset
#' @param dataset2 another CrunchDataset, or possibly a data.frame. If
#' `dataset2` is not a Crunch dataset, it will be uploaded as a new
#' dataset before appending. If it is a CrunchDataset, it may be subsetted with
#' a filter expression on the rows and a selection of variables on the columns.
#' @param autorollback Deprecated. This option no longer does anything.
#' @return `dataset1`, updated with `dataset2`, potentially filtered on rows and
#' variables, appended to it.
#' @examples
#' \dontrun{
#' ds <- loadDataset("Survey, 2016")
#' new_wave <- loadDataset("Survey, 2017")
#' ds <- appendDataset(ds, new_wave)
#' }
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
