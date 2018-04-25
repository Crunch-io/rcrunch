forks <- function (dataset) {
    stopifnot(is.dataset(dataset))
    return(ForkCatalog(crGET(shojiURL(dataset, "catalogs", "forks"))))
}

#' Create a fork of a dataset
#'
#' Forking a dataset allows you to work with a dataset in a way which doesn't
#' affect the other users of that data. A common strategy is to fork a dataset,
#' make changes and then merge those changes back into the original dataset.
#' This is an especially useful workflow when you want to edit a dataset which
#' is shared with clients, and you don't want to accidentally send them
#' incorrect data. By working on a forked dataset you can verify that all of
#' your changes are correct before merging the forked dataset into the
#' client-facing dataset. For more see \code{vignette("fork-and-merge", package
#' = "crunch")}.
#'
#' @param dataset The `CrunchDataset` to fork
#' @param name character name to give the fork. If omitted, one will be provided
#'   for you
#' @param draft logical: Should the dataset be a draft, available only to
#'   editors? Default is `FALSE`.
#' @param ... Additional dataset metadata
#' @return The new fork, a `CrunchDataset`.
#' @seealso [mergeFork()]
#' @export
forkDataset <- function (dataset, name=defaultForkName(dataset), draft=FALSE, ...) {
    ## TODO: add owner field, default to self(me())
    fork_url <- crPOST(shojiURL(dataset, "catalogs", "forks"),
        body=toJSON(wrapEntity(name=name, is_published=!draft, ...)))
    dropOnly(sessionURL("datasets"))
    invisible(entity(datasets()[[fork_url]]))
}

defaultForkName <- function (dataset) {
    nforks <- length(forks(dataset))
    prefix <- ifelse(nforks, paste0("Fork #", nforks + 1, " of"), "Fork of")
    return(paste(prefix, name(dataset)))
}

#'Merge changes to a dataset from a fork
#'
#'Crunch datasets include information about the dataset's revision history. For
#'instance if you add a variable to the dataset there's a record of the action
#'of adding that variable. The sum of these records is a dataset's revision
#'history and it's possible to combine two datasets together by merging their
#'revision histories.
#'
#'This function combines the revision history of two datasets into a single
#'dataset. It is most often used in conjunction with `forkDataset()` to create a
#'copy of a dataset, make some changes to that copy, and then merge the changes
#'back into the original dataset. For more on this workflow see
#'\code{vignette("fork-and-merge", package = "crunch")}.
#'
#'@param dataset The `CrunchDataset` to merge to
#'@param fork The `CrunchDataset`, perhaps forked from `dataset`, that is to be
#'  merged in.
#'@param autorollback logical If the merge fails, should `dataset` be restored
#'  to its state prior to the merge, or should it be left in its partially
#'  merged state for debugging and manual fixing? Default is `TRUE`.
#'@param force logical Attempt to push through merge conflicts by dropping all
#'  changes to `dataset` that occurred after `fork` diverged from and take only
#'  the changes from `fork`? Default is `FALSE`. You should only use force=TRUE
#'  after first attempting and failing to merge without forcing.
#'@return `dataset` with changes from `fork` merged to it.
#'@seealso [forkDataset()]
#'@export
mergeFork <- function (dataset, fork, autorollback=TRUE, force=FALSE) {
    prompt <- paste("Force merge discards any additions or edits to the target",
        "dataset that occurred after the point the fork was created. It cannot",
        "be reverted or otherwise undone. Are you sure you want to continue?")
    if (force && !askForPermission(prompt)) {
        halt("Must confirm force merge")
    }
    payload <- wrapEntity(dataset=self(fork), autorollback=autorollback,
        force=force)
    m <- crPOST(shojiURL(dataset, "catalogs", "actions"), body=toJSON(payload))
    return(refresh(dataset))
}
