#' @rdname crunch-extract
#' @export
setMethod("[[", c("MemberCatalog", "character"), function(x, i, ...) {
    ## TODO: eliminate duplication with PermissionCatalog
    w <- whichCatalogEntry(x, i, ...)
    tup <- x[[w]]
    if (!is.null(tup)) {
        ## TODO: MemberTuple?
        tup <- ShojiTuple(index_url = self(x), entity_url = urls(x)[w], body = tup)
    }
    return(tup)
})

## Alternative lookup in emails, not names
setMethod(
    "whichCatalogEntry", "MemberCatalog",
    function(x, i, ...) whichNameOrURL(x, i, emails(x))
)

#' @rdname crunch-extract
#' @export
setMethod("[[<-", c("MemberCatalog", "ANY", "missing", "ANY"), .backstopUpdate)

#' @rdname crunch-extract
#' @export
setMethod(
    "[[<-", c("MemberCatalog", "character", "missing", "NULL"),
    function(x, i, j, value) {
        ## Remove the specified user from the catalog
        payload <- sapply(i, null, simplify = FALSE)
        crPATCH(self(x), body = toJSON(payload))
        return(refresh(x))
    }
)
