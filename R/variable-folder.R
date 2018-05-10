folders <- function (x) {
    stopifnot(is.dataset(x))
    # folder_url <- try(shojiURL(x, "catalogs", "folders"), silent=TRUE)
    folder_url <- getRootFolderURL(x)
    return(VariableFolder(crGET(folder_url)))
}

getRootFolderURL <- function (dataset) {
    ## Temporary behavior while folders are feature-flagged on the server. We
    ## want to turn on the flag whenever a user attempts to access folders, if
    ## folders aren't already enabled.
    ## Three scenarios:
    ## 1) Everything has folders and there is no more feature flag anymore
    ## 2) Folder flag is already enabled but `dataset` hasn't been refreshed to
    ## have the URL yet
    ## 3) Folder flag is not yet enabled.

    ## First, check for the root folder URL. This will error if there is no
    ## such URL on `dataset`, which there won't be unless the flag was on when
    ## the dataset was loaded.
    folder_url <- try(shojiURL(dataset, "catalogs", "folders"), silent=TRUE)
    if (is.error(folder_url)) {
        ## Next, check flag. It's possible we set the flag in a previous call
        ## to this function, but `dataset` is stale because this function can't
        ## return a modified version of `dataset`.
        if (!isTRUE(settings(dataset)$variable_folders)) {
            ## If the flag is off, turn it on and bust cache on dataset entity
            settings(dataset)$variable_folders <- TRUE
            dropOnly(self(dataset))
        }
        ## GET a fresh dataset entity, which should have the URLs now. This will
        ## read from HTTP cache except the first time after the flag is set
        ## because that drops the cache (L23 above)
        dataset <- ShojiEntity(crGET(self(dataset)))
        folder_url <- shojiURL(dataset, "catalogs", "folders")
    }
    return(folder_url)
}

#' @export
#' @rdname describe-catalog
setMethod("aliases", "VariableFolder", function (x) getIndexSlot(x, "alias"))

setMethod("folderExtraction", "VariableFolder", function (x, tuple) {
    ## "tuple" is a list of length 1, name is URL, contents is the actual tuple
    url <- names(tuple)
    tuple <- tuple[[1]]
    if (tuple$type == "folder") {
        return(VariableFolder(crGET(url)))
    } else {
        tup <- VariableTuple(entity_url=url, body=tuple, index_url=self(x))
        return(CrunchVariable(tup))
    }
})

## Get variable by alias, name, or URL
whichFolderEntry <- function (x, i) {
    ## First check URLs and names()
    out <- whichNameOrURL(x, i, names(x))
    ## Now check variable aliases, if any missing
    not_found <- is.na(out)
    if (any(not_found)) {
        out[not_found] <- match(i[not_found], aliases(x))
    }
    return(out)
}

setMethod("whichCatalogEntry", "VariableFolder",
    function (x, i, ...) whichFolderEntry(x, i))
