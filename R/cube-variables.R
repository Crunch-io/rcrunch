#' @rdname cube-methods
#' @export
setMethod("measures", "CrunchCube", function(x) {
    ## Drop "count" measures, which lack metadata
    ind <- Filter(
        function(a) !is.null(a) && !is.null(a$alias),
        lapply(x@arrays, attr, which = "variable")
    )
    return(.pseudoCatalog(ind))
})

.pseudoCatalog <- function(index) {
    ## Given a list of tuples, fake out enough so regular Catalog methods work
    names(index) <- as.character(seq_along(index)) ## Names just have to exist
    return(VariableCatalog(index = index, self = "")) ## Self just has to exist
}

#' @rdname describe-catalog
#' @export
setMethod("names", "CrunchCube", function(x) names(variables(x)))

#' @rdname describe-catalog
#' @export
setMethod("aliases", "CrunchCube", function(x) aliases(variables(x)))

#' @rdname describe-catalog
#' @export
setMethod("descriptions", "CrunchCube", function(x) descriptions(variables(x)))

#' @rdname describe-catalog
#' @export
setMethod("types", "CrunchCube", function(x) types(variables(x)))

#' @rdname describe-catalog
#' @export
setMethod("notes", "CrunchCube", function(x) notes(variables(x)))
