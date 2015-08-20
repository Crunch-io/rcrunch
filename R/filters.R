setMethod("filters", "CrunchDataset", function (x) {
    FilterCatalog(crGET(shojiURL(x, "catalogs", "filters")))
})

setMethod("filters<-", "CrunchDataset", function (x, value) x)

##' @rdname describe-catalog
##' @export
setMethod("names", "FilterCatalog", function (x) getIndexSlot(x, "name"))

##' @rdname catalog-extract
##' @export
setMethod("[[", c("FilterCatalog", "character"), function (x, i, ...) {
    stopifnot(length(i) == 1)
    z <- match(i, names(x))
    if (is.na(z)) {
        return(NULL)
    }
    return(x[[z]])
})

##' @rdname catalog-extract
##' @export
setMethod("[[", c("FilterCatalog", "numeric"), function (x, i, ...) {
    stopifnot(length(i) == 1)
    url <- urls(x)[i]
    return(CrunchFilter(crGET(url)))
})

##' @rdname catalog-extract
##' @export
setMethod("[[<-", c("FilterCatalog", "character", "missing", "CrunchLogicalExpr"),
    function (x, i, j, value) {
        if (i %in% names(x)) {
            ## TODO: update filter with new expression
            halt("Cannot (yet) modify filter")
        } else {
            ## Creating a new filter
            u <- crPOST(self(x), body=toJSON(list(name=i,
                expression=zcl(value))))
            return(refresh(x))
        }
    })


setMethod("appliedFilters", "CrunchDataset", function (x) {
    ShojiView(crGET(shojiURL(x, "views", "applied_filters")))@value$groups[[1]]$entities
})

setMethod("appliedFilters<-", c("CrunchDataset", "CrunchFilter"),
    function (x, value) {
        b <- toJSON(list(
            groups=I(list(
                list(
                    group="default",
                    entities=I(list(self(value)))
                )
            ))
        ))
        crPUT(shojiURL(x, "views", "applied_filters"), body=b)
        return(x)
    })

setMethod("activeFilter", "CrunchDataset", function (x) x@filter)

setMethod("activeFilter<-", c("CrunchDataset", "CrunchLogicalExpr"), 
    function (x, value) {
        x@filter <- value
        return(x)
    })