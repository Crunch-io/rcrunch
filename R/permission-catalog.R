#' @rdname crunch-extract
#' @export
setMethod("[[", c("PermissionCatalog", "character"), function(x, i, ...) {
    ## TODO: eliminate duplication with MemberCatalog
    w <- whichCatalogEntry(x, i, ...)
    tup <- x[[w]]
    if (!is.null(tup)) {
        ## TODO: MemberTuple?
        tup <- PermissionTuple(index_url = self(x), entity_url = urls(x)[w], body = tup)
    }
    return(tup)
})

## Alternative lookup in emails, not names
setMethod(
    "whichCatalogEntry", "PermissionCatalog",
    function(x, i, ...) whichNameOrURL(x, i, emails(x))
)
