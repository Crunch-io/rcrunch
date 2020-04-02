setMethod("tuple", "CrunchVariable", function(x) x@tuple)
setMethod("tuple<-", "CrunchVariable", function(x, value) {
    x@tuple <- value
    return(x)
})

#' @rdname self
#' @export
setMethod("self", "CrunchVariable", function(x) tuple(x)@entity_url)

#' @rdname refresh
#' @export
setMethod("refresh", "CrunchVariable", function(x) {
    return(CrunchVariable(refresh(tuple(x)), filter = activeFilter(x)))
})

#' @rdname describe-entity
#' @export
setMethod("alias", "VariableTuple", function(object) object$alias)
#' @rdname describe-entity
#' @export
setMethod("description", "VariableTuple", function(x) x$description %||% "")
#' @rdname describe-entity
#' @export
setMethod("notes", "VariableTuple", function(x) x$notes %||% "")

#' @rdname describe-entity
#' @export
setMethod("name", "CrunchVariable", function(x) name(tuple(x)))
#' @rdname describe-entity
#' @export
setMethod(
    "name<-", "CrunchVariable",
    function(x, value) setTupleSlot(x, "name", validateNewName(value))
)
#' @rdname describe-entity
#' @export
setMethod("id", "CrunchVariable", function(x) {
    return(tuple(x)$id)
})
#' @rdname describe-entity
#' @export
setMethod("description", "CrunchVariable", function(x) description(tuple(x)))
#' @rdname describe-entity
#' @export
setMethod(
    "description<-", "CrunchVariable",
    function(x, value) setTupleSlot(x, "description", value %||% "")
)
#' @rdname describe-entity
#' @export
setMethod("alias", "CrunchVariable", function(object) alias(tuple(object)))
#' @rdname describe-entity
#' @export
setMethod(
    "alias<-", "CrunchVariable",
    function(x, value) setTupleSlot(x, "alias", validateNewName(value))
)
#' @rdname describe-entity
#' @export
setMethod("notes", "CrunchVariable", function(x) notes(tuple(x)))
#' @rdname describe-entity
#' @export
setMethod(
    "notes<-", "CrunchVariable",
    function(x, value) setTupleSlot(x, "notes", value %||% "")
)

#' @rdname describe-entity
#' @export
setMethod("digits", "CrunchVariable", function(x) {
    var_entity <- entity(x)
    return(var_entity@body$format$data$digits)
})
#' @rdname describe-entity
#' @export
setMethod("digits<-", "NumericVariable", function(x, value) {
    if (!is.numeric(value) || !is.whole(value)) {
        halt("digit specifications should be an integer")
    }
    if (value < 0 | value > 16) {
        halt("digit specifications should be between 0 and 16")
    }

    frmt <- wrapEntity("format" = list("data" = list("digits" = value)))
    crPATCH(self(x), body = toJSON(frmt))
    invisible(x)
})
#' @rdname describe-entity
#' @export
setMethod("digits<-", "CrunchVariable", function(x, value) {
    halt("digit specifications can only be set for numeric variables")
})

#' @rdname describe-entity
#' @export
setMethod("length", "CrunchVariable", function (x) {
    ds <- loadDataset(datasetReference(x))
    activeFilter(ds) <- activeFilter(x)
    return(nrow(ds))
})

#' Split an array or multiple-response variable into its CategoricalVariables
#'
#' @param x a `CategoricalArrayVariable` or `MultipleResponseVariable`
#' @return invisibly, the API response from DELETEing the array variable
#' definition. If you [refresh()] the corresponding dataset after
#' unbinding, you should see the array variable removed and its subvariables
#' promoted to regular variables.
#' @export
unbind <- function(x) {
    stopifnot(inherits(x, "CategoricalArrayVariable"))
    ## Delete self and drop cache for variable catalog (parent)
    u <- self(x)
    out <- crPOST(u, body = '{"unbind": {}}')
    dropCache(datasetReference(u))
    invisible(out)
}

#' @rdname crunch-extract
#' @export
setMethod("[", c("CrunchVariable", "CrunchExpr"), .updateActiveFilter)
#' @rdname crunch-extract
#' @export
setMethod("[", c("CrunchVariable", "numeric"), function(x, i, ...) {
    filt <- activeFilter(x)
    if (!is.null(filt)) {
        return(harmonizeFilters(x, filt, i))
    } else {
        return(x[seq_len(length(x)) %in% i])
    }
})
#' @rdname crunch-extract
#' @export
setMethod("[", c("CrunchVariable", "logical"), .updateActiveFilterLogical)


# for getting and setting the uniform_basis property of multiple response variables.
#' @rdname describe-entity
#' @export
setMethod("uniformBasis", "MultipleResponseVariable", function(x) tuple(x)$uniform_basis)
#' @rdname describe-entity
#' @export
setMethod("uniformBasis<-", "MultipleResponseVariable", function(x, value) {
    stopifnot(is.TRUEorFALSE(value))
    # drop cube cache, since this will change the way they are executed
    dropCache(cubeURL(datasetReference(x)))
    return(setTupleSlot(x, "uniform_basis", value))
})
