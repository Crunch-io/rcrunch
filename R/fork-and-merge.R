forks <- function (dataset) {
    stopifnot(is.dataset(dataset))
    return(ForkCatalog(crGET(shojiURL(dataset, "catalogs", "forks"))))
}

##' Create a fork of a dataset
##'
##' As with many other version control systems, in Crunch you can fork a
##' dataset's revision history, effectively making a copy on which you can work
##' independently of the original dataset. You can then merge those change back
##' to the original dataset or keep working independently.
##' @param dataset The \code{CrunchDataset} to fork
##' @param name character name to give the fork. If omitted, one will be
##' provided for you
##' @param draft logical: Should the dataset be a draft, available only to
##' editors? Default is \code{FALSE}.
##' @param ... Additional dataset metadata
##' @return The new fork, a \code{CrunchDataset}.
##' @export
forkDataset <- function (dataset, name=defaultForkName(dataset), draft=FALSE, ...) {
    fork_url <- crPOST(shojiURL(dataset, "catalogs", "forks"),
        body=toJSON(list(element="shoji:entity",
                         body=list(name=name, is_published=!draft, ...))))
    dropOnly(sessionURL("datasets"))
    invisible(entity(datasetCatalog()[[fork_url]]))
}

defaultForkName <- function (dataset) {
    nforks <- length(forks(dataset))
    prefix <- ifelse(nforks, paste0("Fork #", nforks + 1, " of"), "Fork of")
    return(paste(prefix, name(dataset)))
}

##' Merge changes to a dataset from a fork
##'
##' @param dataset The \code{CrunchDataset} to merge to
##' @param fork The \code{CrunchDataset}, perhaps forked from \code{dataset},
##' that is to be merged in.
##' @param autorollback logical If the merge fails, should \code{dataset} be
##' restored to its state prior to the merge, or should it be left in its
##' partially merged state for debugging and manual fixing? Default is
##' \code{TRUE}, i.e. the former.
##' @return \code{dataset} with changes from \code{fork} merged to it.
##' @export
mergeFork <- function (dataset, fork, autorollback=TRUE) {
    m <- crPOST(shojiURL(dataset, "catalogs", "actions"), body=toJSON(list(
        element="shoji:entity",
        body=list(dataset=self(fork), autorollback=autorollback)
    )))
    return(refresh(dataset))
}
