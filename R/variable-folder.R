#' @rdname describe-entity
#' @export
setMethod(
    "name<-", "VariableFolder",
    function(x, value) setEntitySlotWrapBody(x, "name", value)
)

#' @export
#' @rdname describe-catalog
setMethod("aliases", "VariableFolder", function(x) getIndexSlot(x, "alias"))

setMethod("folderExtraction", "VariableFolder", function(x, tuple) {
    ## "tuple" is a list of length 1, name is URL, contents is the actual tuple
    url <- names(tuple)
    tuple <- tuple[[1]]
    if (is.null(tuple$type)) {
        halt("Could not find type for item in folder: ", url) #nocov
    } else if (tuple$type == "folder") {
        return(VariableFolder(crGET(url)))
    } else {
        tup <- VariableTuple(entity_url = url, body = tuple, index_url = self(x))
        return(CrunchVariable(tup))
    }
})

# Walk up the folders above folder containing the variable so that you
# get to the right folder whether it be public/hidden/private
setMethod("rootFolder", "CrunchVariable", function(x) rootFolder(folder(x)))

## Get variable by alias, name, or URL
whichFolderEntry <- function(x, i) {
    ## First check URLs and names()
    out <- whichNameOrURL(x, i, names(x))
    ## Now check variable aliases, if any missing
    not_found <- is.na(out)
    if (any(not_found)) {
        out[not_found] <- match(i[not_found], aliases(x))
    }
    return(out)
}

setMethod(
    "whichCatalogEntry", "VariableFolder",
    function(x, i, ...) whichFolderEntry(x, i)
)
