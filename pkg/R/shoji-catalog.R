is.shojiCatalog <- function (x) inherits(x, "ShojiCatalog")

setIndexSlot <- function (x, i, value) {
    x@index <- lapply(x, function (a) {
        a[[i]] <- value
        return(a)
    })
    PATCH(self(x), body=toJSON(x@index))
    return(x)
}

setMethod("[", c("ShojiCatalog", "ANY"), function (x, i, ..., drop) {
   x@index <- x@index[i]
   return(x)
})
setMethod("length", "ShojiCatalog", function (x) length(x@index))
setMethod("lapply", "ShojiCatalog", function (X, FUN, ...) lapply(X@index, FUN, ...))

urls <- function (x) {
    names(x@index)
}

##' @S3method as.list ShojiCatalog
as.list.ShojiCatalog <- function (x, ...) lapply(names(x@index), function (i) x[[i]])