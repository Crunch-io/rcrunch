setMethod("initialize", "MultitableCatalog", init.sortCatalog)

multitables <- function (dataset) {
    MultitableCatalog(crGET(shojiURL(dataset, "catalogs", "multitables")))
}

#' @rdname catalog-extract
#' @export
setMethod("[[", c("MultitableCatalog", "numeric"), function (x, i, ...) {
    getEntity(x, i, Multitable, ...)
})

setMethod("is.public", "MultitableCatalog", function (x) {
    getIndexSlot(x, "is_public", what=logical(1))
})

setMethod("is.public<-", "MultitableCatalog", function (x, value) {
    setIndexSlot(x, "is_public", value)
})

newMultitable <- function (formula, data) {

}
