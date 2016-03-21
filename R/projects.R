##' @rdname catalog-extract
##' @export
setMethod("[[", c("ProjectCatalog", "character"), function (x, i, ...) {
    w <- whichNameOrURL(x, i)
    x[[w]]
})

##' @rdname catalog-extract
##' @export
setMethod("[[", c("ProjectCatalog", "ANY"), function (x, i, ...) {
    b <- callNextMethod(x, i, ...)
    if (is.null(b)) return(NULL)
    CrunchProject(index_url=self(x), entity_url=urls(x)[i],
        body=b)
})

##' @rdname teams
##' @export
setMethod("members", "CrunchProject", function (x) {
    MemberCatalog(crGET(shojiURL(entity(x), "catalogs", "members")))
})

##' @rdname tuple-methods
##' @export
setMethod("entity", "CrunchProject", function (x) {
    return(ProjectEntity(crGET(x@entity_url)))
})
