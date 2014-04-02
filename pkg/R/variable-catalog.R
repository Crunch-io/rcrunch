init.VariableCatalog <- function (.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@order <- do.call(VariableGrouping,
        GET(.Object@views$hierarchical_order)$groups)
    return(.Object)
}
setMethod("initialize", "VariableCatalog", init.VariableCatalog)

setMethod("ordering", "VariableCatalog", function (x) x@order)