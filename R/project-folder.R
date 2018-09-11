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

#' @rdname teams
#' @export
setMethod("members", "ProjectFolder", function(x) {
    MemberCatalog(crGET(shojiURL(x, "catalogs", "members")))
})

#' @rdname teams
#' @export
setMethod("members<-", c("ProjectFolder", "MemberCatalog"), function(x, value) {
    ## TODO: something
    ## For now, assume action already done in other methods, like NULL
    ## assignment above.
    return(x)
})

#' @rdname teams
#' @export
setMethod("members<-", c("ProjectFolder", "character"), function(x, value) {
    value <- setdiff(value, emails(members(x)))
    if (length(value)) {
        payload <- sapply(value, emptyObject, simplify = FALSE)
        crPATCH(self(members(x)), body = toJSON(payload))
    }
    return(x)
})
