##' Clone a dataset
##'
##' This function makes a copy of a Crunch Dataset and its associated variables
##' and variable ordering. It does not copy filters, saved analyses, sharing
##' privileges, etc.
##' @param dataset CrunchDataset to clone
##' @param useAlias logical whether variable alias or name should be used as R
##' variable names when the dataset is returned. Default is TRUE, meaning alias.
##' @param ... additional arguments, currently ignored
##' @return a CrunchDataset cloned from \code{dataset}
##' @export
cloneDataset <- function (dataset, useAlias=default.useAlias(), ...) {
    payload <- list(clone_from=self(dataset))
    dataset_url <- POST(sessionURL("datasets_url"), body=toJSON(payload))
    updateDatasetList()
    clone <- entity(datasetCatalog()[[dataset_url]])
    clone@useAlias <- useAlias
    invisible(clone)
}

##' Graft data from a cloned dataset back to its source
##'
##' This lets you prepare edits for a dataset on a clone of the dataset, then 
##' copy that data back to the original dataset once you have verified that the
##' edits are what you want to share with other users of the dataset. 
##'
##' As the warning message in the function states, please be absolutely sure
##' that you know what you're doing before you run this function. Grafting data
##' that was not originally cloned from the same dataset, could have 
##' disastrous consequences for your dataset. Human sacrifice, dogs and cats
##' living together, mass hysteria. Proceed with caution.
##'
##' @param primary the CrunchDataset onto which you wish to graft data
##' @param clone the CrunchDataset you wish to take data from to graft onto
##' \code{primary}
##' @return \code{primary} with the data from \code{clone} grafted onto it
##' @export
graftDataset <- function (primary, clone) {
    stopifnot(is.dataset(primary), is.dataset(clone))
    message(paste("Grafting should only be done if one dataset is a clone of",
        "the other. Note also that if variables have been deleted from the",
        "data you are grafting, any filters, analyses, etc. in your primary",
        "dataset that depend on those variables will not work."))
    msg <- paste0("Take data from ", dQuote(name(clone)), 
        " and graft it onto ", dQuote(name(primary)), "?")
    if (askForPermission(msg)) {
        payload <- list(command="graft", dataset=self(clone))
        POST(self(primary), body=toJSON(payload))
    } else {
        halt("Permission to graft not given. Aborting")
    }
    invisible(refresh(primary))
}