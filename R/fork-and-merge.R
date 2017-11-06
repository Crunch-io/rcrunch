forks <- function (dataset) {
    stopifnot(is.dataset(dataset))
    return(ForkCatalog(crGET(shojiURL(dataset, "catalogs", "forks"))))
}

#' Create a fork of a dataset
#'
#' As with many other version control systems, in Crunch you can fork a
#' dataset's revision history, effectively making a copy on which you can work
#' independently of the original dataset. You can then merge those change back
#' to the original dataset or keep working independently.
#' @param dataset The `CrunchDataset` to fork
#' @param name character name to give the fork. If omitted, one will be
#' provided for you
#' @param draft logical: Should the dataset be a draft, available only to
#' editors? Default is `FALSE`.
#' @param ... Additional dataset metadata
#' @return The new fork, a `CrunchDataset`.
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

#' Merge changes to a dataset from a fork
#'
#' @param dataset The `CrunchDataset` to merge to
#' @param fork The `CrunchDataset`, perhaps forked from `dataset`,
#' that is to be merged in.
#' @param autorollback logical If the merge fails, should `dataset` be
#' restored to its state prior to the merge, or should it be left in its
#' partially merged state for debugging and manual fixing? Default is
#'`TRUE`.
#' @param force logical Attempt to push through merge conflicts by dropping
#' all changes to `dataset` that occurred after `fork` diverged from and take only
#' the changes from `fork`? Default is `FALSE`. You should only use force=TRUE after
#' first attempting and failing to merge without forcing.
#' @return `dataset` with changes from `fork` merged to it.
#' @export
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
