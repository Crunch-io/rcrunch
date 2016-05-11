.backstopUpdate <- function (x, i, j, value) {
    ## Backstop error so you don't get "Object of class S4 is not subsettable"
    halt(paste("Cannot update", class(x), "with type", class(value)))
}

##' @rdname catalog-extract
##' @export
setMethod("[[<-", c("MemberCatalog", "ANY", "missing", "ANY"), .backstopUpdate)

##' @rdname catalog-extract
##' @export
setMethod("[[<-", c("MemberCatalog", "character", "missing", "NULL"),
    function (x, i, j, value) {
        ## Remove the specified user from the catalog
        payload <- sapply(i, null, simplify=FALSE)
        crPATCH(self(x), body=toJSON(payload))
        return(refresh(x))
    })

##' @rdname teams
##' @export
setMethod("members<-", c("CrunchTeam", "MemberCatalog"), function (x, value) {
    ## TODO: something
    ## For now, assume action already done in other methods, like NULL
    ## assignment above.
    return(x)
})

##' @rdname teams
##' @export
setMethod("members<-", c("CrunchTeam", "character"), function (x, value) {
    payload <- sapply(value, emptyObject, simplify=FALSE)
    crPATCH(self(members(x)), body=toJSON(payload))
    return(refresh(x))
})

setMethod("is.editor", "MemberCatalog", function (x) {
    ## N.B.: this is for projects; teams don't work this way.
    vapply(index(x), function (a) {
            isTRUE(a[["permissions"]][["edit"]])
        }, logical(1), USE.NAMES=FALSE)
})

setMethod("is.editor<-", c("MemberCatalog", "logical"), function (x, value) {
    stopifnot(length(x) == length(value))
    changed <- is.editor(x) != value
    if (any(changed)) {
        payload <- structure(lapply(value[changed], {
            function (v) list(permissions=list(edit=v))
        }), .Names=urls(x)[changed])
        crPATCH(self(x), body=toJSON(payload))
        x <- refresh(x)
    }
    return(x)
})
