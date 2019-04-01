setMethod("folderExtraction", "ProjectFolder", function(x, tuple) {
    ## "tuple" is a list of length 1, name is URL, contents is the actual tuple
    url <- names(tuple)
    tuple <- tuple[[1]]
    if (tuple$type == "project") {
        return(ProjectFolder(crGET(url)))
    } else {
        return(loadDatasetFromURL(url))
    }
})

#' @rdname describe
#' @export
setMethod("name", "ProjectFolder", function(x) {
    ## Warning: bad code smell
    if (identical(self(x), sessionURL("projects"))) {
        ## Root, so return no name
        return("")
    } else {
        callNextMethod(x)
    }
})

setMethod("personalFolder", "ProjectFolder", function(x) {
    root <- projects()
    return(ProjectFolder(crGET(shojiURL(root, "catalogs", "personal"))))
})

setMethod("rootFolder", "ProjectFolder", function(x) projects())

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

#' @rdname crunch-extract
#' @export
setMethod("[[<-", c("ProjectFolder", "character", "missing", "list"),
    function(x, i, j, value) {
        # This is for backwards compatibility with the old project API
        if (i %in% names(x)) {
            ## TODO: update team attributes
            halt("Cannot (yet) modify project attributes")
        } else {
            ## Creating a new project
            proj <- do.call(
                newProject,
                modifyList(value, list(name = i, catalog = x))
            )
            return(refresh(x))
        }
    }
)

#' @rdname crunch-extract
#' @export
setMethod("[[<-", c("ProjectFolder", "character", "missing", "ProjectFolder"),
    function(x, i, j, value) {
        # This is for backwards compatibility with the old project API

        ## Assumes that modifications have already been persisted
        ## by other operations on the team entity (like members<-)
        if (i %in% names(x) && identical(self(x[[i]]), self(value))) {
            # TODO: could patch up the tuple with the entity
            return(refresh(x))
        } else {
            halt("Not implemented")
        }
    }
)

setMethod("active", "ProjectFolder", function(x) {
    index(x) <- Filter(function(a) !isTRUE(a$archived), index(x))
    return(x)
})

setMethod("archived", "ProjectFolder", function(x) {
    index(x) <- Filter(function(a) isTRUE(a$archived), index(x))
    return(x)
})
