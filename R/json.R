#' @rdname tojson-crunch
#' @export
setMethod("jsonprep", "AbstractCategories", function (x, ...) jsonprep(I(x@.Data)))

#' @rdname tojson-crunch
#' @export
setMethod("jsonprep", "ANY", function (x, ...) {
    if (is.list(x)) {
        ## Do this so that any S3 objects are supported (they are is.list==TRUE
        ## but don't dispatch S3/S4 methods as list)
        x <- lapply(x, jsonprep, ...)
    }
    return(x)
})


.jsonprep.ordergroup <- function (x, ...) {
    ents <- x@entities
    if (length(ents) == 0) {
        ## toJSON(character(0)) is [""], which is length 1 :(
        ents <- list() ## but toJSON(list()) is []
    } else if (is.list(ents)) {
        nested.groups <- vapply(ents, inherits, logical(1),
            what="OrderGroup")
        ents[nested.groups] <- lapply(ents[nested.groups], .jsonprep.ordergroup)
    }
    return(structure(list(I(ents)), .Names=x@group))
}

#' @rdname tojson-crunch
#' @export
setMethod("jsonprep", "ShojiOrder",
    function (x, ...) jsonprep(list(graph=x@graph, ...)))

#' @rdname tojson-crunch
#' @export
setMethod("jsonprep", "OrderGroup", .jsonprep.ordergroup)


#' @importFrom jsonlite toJSON
#' @rdname tojson-crunch
#' @export
toJSON <- function (x, ...) {
    if (is.null(x)) return(jsonlite::toJSON(emptyObject()))
    out <- jsonlite::toJSON(jsonprep(x), auto_unbox=TRUE, null="null", na="null", force=TRUE, ...)
    # cat(out)
    return(out)
}
