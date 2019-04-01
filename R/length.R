#' Length of Crunch objects
#' @param x a Crunch object (catalog, order, etc.)
#' @return Integer: the number of elements in the catalog, group, folder, etc.
#' @name crunch-length
setGeneric("length")

#' @rdname crunch-length
#' @export
setMethod("length", "CrunchDeck", function(x) return(length(slides(x))))

#' @rdname crunch-length
#' @export
setMethod("length", "ShojiCatalog", function(x) length(index(x)))

#' @rdname crunch-length
#' @export
setMethod("length", "ShojiOrder", function(x) length(entities(x)))

#' @rdname crunch-length
#' @export
setMethod("length", "OrderGroup", function(x) length(entities(x)))

#' @rdname crunch-length
#' @export
setMethod("length", "TabBookResult", function(x) length(x$sheets))

#' @rdname crunch-length
#' @export
setMethod("length", "MultitableResult", function(x) length(x$result))
