#' @rdname catalog-extract
#' @export
setMethod("[[", c("PermissionCatalog", "character"), function (x, i, ...) {
    stopifnot(length(i) == 1L)
    w <- whichNameOrURL(x, i, emails(x))
    if (is.na(w)) {
        halt("Subscript out of bounds: ", i)
    }
    tup <- index(x)[[w]]
    return(PermissionTuple(index_url=self(x), entity_url=urls(x)[w],
        body=tup))
})

#' @rdname catalog-extract
#' @export
setMethod("[", c("PermissionCatalog", "character"), function (x, i, ...) {
    w <- whichNameOrURL(x, i, emails(x))
    if (any(is.na(w))) {
        halt("Undefined elements selected: ", serialPaste(i[is.na(w)]))
    }
    return(x[w])
})
