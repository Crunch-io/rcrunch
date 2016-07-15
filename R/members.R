#' @rdname catalog-extract
#' @export
setMethod("[[", c("MemberCatalog", "character"), function (x, i, ...) {
    ## TODO: eliminate duplication with PermissionCatalog
    stopifnot(length(i) == 1L)
    w <- whichNameOrURL(x, i, emails(x))
    if (is.na(w)) {
        halt("Subscript out of bounds: ", i)
    }
    tup <- index(x)[[w]]
    return(ShojiTuple(index_url=self(x), entity_url=urls(x)[w],
        body=tup)) ## TODO: MemberTuple
})

#' @rdname catalog-extract
#' @export
setMethod("[", c("MemberCatalog", "character"), function (x, i, ...) {
    w <- whichNameOrURL(x, i, emails(x))
    if (any(is.na(w))) {
        halt("Undefined elements selected: ", serialPaste(i[is.na(w)]))
    }
    return(x[w])
})



#' @rdname catalog-extract
#' @export
setMethod("[[<-", c("MemberCatalog", "ANY", "missing", "ANY"), .backstopUpdate)

#' @rdname catalog-extract
#' @export
setMethod("[[<-", c("MemberCatalog", "character", "missing", "NULL"),
    function (x, i, j, value) {
        ## Remove the specified user from the catalog
        payload <- sapply(i, null, simplify=FALSE)
        crPATCH(self(x), body=toJSON(payload))
        return(refresh(x))
    })

#' @rdname teams
#' @export
setMethod("members<-", c("CrunchTeam", "MemberCatalog"), function (x, value) {
    ## TODO: something
    ## For now, assume action already done in other methods, like NULL
    ## assignment above.
    return(x)
})

#' @rdname teams
#' @export
setMethod("members<-", c("CrunchTeam", "character"), function (x, value) {
    payload <- sapply(value, emptyObject, simplify=FALSE)
    crPATCH(self(members(x)), body=toJSON(payload))
    return(refresh(x))
})

#' Read and set edit privileges
#'
#' @param x PermissionCatalog or MemberCatalog
#' @param value For the setter, logical: should the indicated users be allowed
#' to edit the associated object?
#' @return \code{is.editor} returns a logical vector corresponding to whether
#' the users in the catalog can edit or not. \code{is.editor<-} returns the
#' catalog, modified.
#' @name is.editor
#' @aliases is.editor is.editor<-
NULL

#' @rdname is.editor
#' @export
setMethod("is.editor", "MemberCatalog", function (x) {
    ## N.B.: this is for projects; teams don't work this way.
    vapply(index(x), function (a) {
            isTRUE(a[["permissions"]][["edit"]])
        }, logical(1), USE.NAMES=FALSE)
})

#' @rdname is.editor
#' @export
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
