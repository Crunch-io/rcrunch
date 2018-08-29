setMethod("folderExtraction", "ProjectFolder", function(x, tuple) {
    ## "tuple" is a list of length 1, name is URL, contents is the actual tuple
    url <- names(tuple)
    tuple <- tuple[[1]]
    if (tuple$type == "project") {
        return(ProjectFolder(crGET(url)))
    } else {
        return(CrunchDataset(tuple))
    }
})
