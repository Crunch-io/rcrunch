#' @rdname cube-methods
#' @export
setMethod("variables", "CubeDims", function (x) {
    ind <- lapply(x@.Data, vget("references"))
    return(.pseudoCatalog(ind))
})

#' @rdname cube-methods
#' @export
setMethod("measures", "CrunchCube", function (x) {
    ind <- Filter(Negate(is.null), ## Drop "count" measures, which lack metadata
        lapply(x@arrays, attr, which="variable"))
    return(.pseudoCatalog(ind))
})

.pseudoCatalog <- function (index) {
    ## Given a list of tuples, fake out enough so regular Catalog methods work
    names(index) <- as.character(seq_along(index)) ## Names just have to exist
    return(VariableCatalog(index=index, self="")) ## Self just has to exist
}

#' @rdname cube-methods
#' @export
setMethod("variables", "CrunchCube", function (x) variables(dimensions(x)))

#' @rdname cube-methods
#' @export
setMethod("names", "CrunchCube", function (x) names(variables(x)))

#' @rdname cube-methods
#' @export
setMethod("aliases", "CrunchCube", function (x) aliases(variables(x)))

#' @rdname cube-methods
#' @export
setMethod("descriptions", "CrunchCube", function (x) descriptions(variables(x)))

#' @rdname cube-methods
#' @export
setMethod("types", "CrunchCube", function (x) types(variables(x)))

#' @rdname cube-methods
#' @export
setMethod("notes", "CrunchCube", function (x) notes(variables(x)))
