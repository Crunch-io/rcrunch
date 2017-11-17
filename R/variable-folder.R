folders <- function (x) {
    stopifnot(is.dataset(x))
    return(VariableFolder(crGET(shojiURL(x, "catalogs", "folders"))))
}

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

## TODO: get variable by alias, name, or URL?
