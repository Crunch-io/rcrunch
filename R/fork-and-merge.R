forks <- function(dataset) {
    stopifnot(is.dataset(dataset))
    return(ForkCatalog(crGET(shojiURL(dataset, "catalogs", "forks"))))
}

#' Create a fork of a dataset
#'
#' Forking a dataset makes a copy of the data that is linked by Crunch's version
#' control system to the original dataset. When you make edits to a fork, users
#' of the original dataset do not see the changes.
#'
#' A common strategy for revising a dataset that has been shared with others is
#' to fork it,
#' make changes to the fork, and then merge those changes back into the original
#' dataset.
#' This workflow allows you to edit a dataset and review changes before
#' publishing them, so that you don't accidentally send your clients
#' incorrect data. For more on this workflow, see
#' `vignette("fork-and-merge", package = "crunch")`.
#'
#' @param dataset The `CrunchDataset` to fork
#' @param name character name to give the fork. If omitted, one will be provided
#' for you
#' @param draft logical: Should the dataset be a draft, visible only to
#' those with edit permissions? Default is `FALSE`.
#' @param ... Additional dataset metadata to provide to the fork
#' @param project A `ProjectFolder` object, string path that could be passed to [`cd()`]
#' relative to the root project, or a URL for a `ProjectFolder`. If left empty,
#' rcrunch will look in `envOrOption('crunch.default.project')` and error if nothing
#' is found.
#' @return The new fork, a `CrunchDataset`.
#' @seealso [mergeFork()]
#' @examples
#' \dontrun{
#' # Defaults to being placed in the same project folder as the original dataset
#' ds_fork <- forkDataset(ds)
#'
#' # But you can specify a project by path, `ProjectFolder` object or URL
#' ds_fork2 <- forkDataset(ds, project = "/Client1/forks/")
#' ds_fork3 <- forkDataset(ds, project = projects()[["My forks"]])
#' ds_fork4 <- forkDataset(ds, project = "https://app.crunch.io/api/projects/abc/")
#' }
#' @export
forkDataset <- function(dataset, name = defaultForkName(dataset), draft = FALSE, ..., project = defaultCrunchProject()) {
    ## TODO: add owner field, default to self(me())
    fork_url <- crPOST(shojiURL(dataset, "catalogs", "forks"),
        body = toJSON(wrapEntity(name = name, is_published = !draft, ..., project = resolveProjectURL(project)))
    )
    dropOnly(sessionURL("datasets"))
    invisible(loadDatasetFromURL(fork_url))
}

defaultForkName <- function(dataset) {
    nforks <- length(forks(dataset))
    prefix <- ifelse(nforks, paste0("Fork #", nforks + 1, " of"), "Fork of")
    return(paste(prefix, name(dataset)))
}

#' Merge changes to a dataset from a fork
#'
#' Crunch datasets include information about the dataset's revision history.
#' This function takes the changes made on a dataset fork and adds them to the
#' revision history of the parent dataset, like a merge of branches in a
#' version control system.
#'
#' All modifications of a dataset record actions in its revision history. For
#' example, if you add a variable to the dataset, that action is recorded. The
#' sum of these records is a dataset's revision history, and it is possible
#' to merge in the revision history of a dataset that has been forked.
#'
#' This function is most often used in conjunction with [forkDataset()] to
#' create a copy of a dataset, make some changes to that copy, and then merge
#' the changes back into the original dataset. For more on this workflow, see
#' `vignette("fork-and-merge", package = "crunch")`.
#'
#' @param dataset The `CrunchDataset` to merge to
#' @param fork The `CrunchDataset`, which must be a fork from `dataset`, that is
#' to be merged in.
#' @param autorollback logical If the merge fails, should `dataset` be restored
#' to its state prior to the merge, or should it be left in its partially
#' merged state for debugging and manual fixing? Default is `TRUE`.
#' @param force logical Attempt to push through merge conflicts by dropping all
#' changes to `dataset` that occurred after `fork` diverged from and take only
#' the changes from `fork`? Default is `FALSE`. You should only use `force=TRUE`
#' after first attempting and failing to merge without forcing.
#' @return `dataset` with changes from `fork` merged to it.
#' @seealso [forkDataset()]
#' @examples
#' \dontrun{
#' ds <- loadDataset("My survey", project = "Studies")
#' fork <- forkDataset(ds)
#' # Do stuff to fork
#' ds <- mergeFork(ds, fork)
#' # Now the changes you did to fork are also on ds
#' }
#' @export
mergeFork <- function(dataset, fork, autorollback = TRUE, force = FALSE) {
    prompt <- paste(
        "Force merge discards any additions or edits to the target",
        "dataset that occurred after the point the fork was created. It cannot",
        "be reverted or otherwise undone. Are you sure you want to continue?"
    )
    if (force && !askForPermission(prompt)) {
        halt("Must confirm force merge")
    }
    payload <- wrapEntity(
        dataset = self(fork), autorollback = autorollback,
        force = force
    )
    crPOST(shojiURL(dataset, "catalogs", "actions"), body = toJSON(payload))
    return(refresh(dataset))
}
