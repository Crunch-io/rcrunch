#' Length of Crunch objects
#' @param x a Crunch object (catalog, order, etc.)
#' @return Integer: the number of elements in the catalog, group, folder, etc.
#' @name length
#' @keywords internal
NULL

setGeneric("length")

#' @rdname length
#' @export
setMethod("length", "CrunchDeck", function(x) return(length(slides(x))))

#' @rdname length
#' @export
setMethod("length", "ShojiCatalog", function(x) length(index(x)))

#' @rdname length
#' @export
setMethod("length", "ShojiOrder", function(x) length(entities(x)))

#' @rdname length
#' @export
setMethod("length", "OrderGroup", function(x) length(entities(x)))

#' @rdname length
#' @export
setMethod("length", "TabBookResult", function(x) length(x$sheets))

#' @rdname length
#' @export
setMethod("length", "MultitableResult", function(x) length(x$result))
