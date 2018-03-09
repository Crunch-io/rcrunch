setMethod("tuple", "CrunchVariable", function (x) x@tuple)
setMethod("tuple<-", "CrunchVariable", function (x, value) {
    x@tuple <- value
    return(x)
})

#' @rdname self
#' @export
setMethod("self", "CrunchVariable", function (x) tuple(x)@entity_url)

#' @rdname refresh
#' @export
setMethod("refresh", "CrunchVariable", function (x) {
    return(CrunchVariable(refresh(tuple(x)), filter=activeFilter(x)))
})

#' @rdname describe
#' @export
setMethod("name", "CrunchVariable", function (x) tuple(x)$name)
#' @rdname describe
#' @export
setMethod("name<-", "CrunchVariable",
    function (x, value) setTupleSlot(x, "name", validateNewName(value)))
#' @rdname describe
#' @export
setMethod("description", "CrunchVariable", function (x) tuple(x)$description)
#' @rdname describe
#' @export
setMethod("description<-", "CrunchVariable",
    function (x, value) setTupleSlot(x, "description", value %||% ""))
#' @rdname describe
#' @export
setMethod("alias", "CrunchVariable", function (object) tuple(object)$alias)
#' @rdname describe
#' @export
setMethod("alias<-", "CrunchVariable",
    function (x, value) setTupleSlot(x, "alias", validateNewName(value)))
#' @rdname describe
#' @export
setMethod("notes", "CrunchVariable", function (x) tuple(x)$notes)
#' @rdname describe
#' @export
setMethod("notes<-", "CrunchVariable",
    function (x, value) setTupleSlot(x, "notes", value %||% ""))

#' @rdname describe
#' @export
setMethod("digits", "CrunchVariable", function (x) {
    var_entity <- entity(x)
    return(var_entity@body$format$data$digits)
})
#' @rdname describe
#' @export
setMethod("digits<-", "NumericVariable", function (x, value) {
    if (!is.numeric(value) || !is.whole(value)) {
        halt("digit specifications should be an integer")
    }
    if (value < 0 | value > 16) {
        halt("digit specifications should be between 0 and 16")
    }

    frmt <- wrapEntity("format" = list("data" = list("digits" = value)))
    crPATCH(self(x), body=toJSON(frmt))
    invisible(x)
})
#' @rdname describe
#' @export
setMethod("digits<-", "CrunchVariable", function (x, value) {
    halt("digit specifications can only be set for numeric variables")
})

#' Get and set Categories on Variables
#'
#' @param x a Variable
#' @param value for the setters, an object of class Categories to set.
#' @return Getters return Categories; setters return \code{x} duly modified.
#' @name var-categories
#' @aliases var-categories categories categories<-
NULL

#' @rdname var-categories
#' @export
setMethod("categories", "VariableTuple", function (x) {
    ## VariableTuples from a regular VariableCatalog don't have categories.
    ## But, from variableMetadata() and from variables(cube), they do. And
    ## if they do, return them instead of making an entity() request.
    cats <- x$categories
    if (!is.null(cats)) {
        cats <- Categories(data=cats)
    }
    return(cats)
})

#' @rdname var-categories
#' @export
setMethod("categories", "CrunchVariable", function (x) categories(tuple(x)))
#' @rdname var-categories
#' @export
setMethod("categories", "CategoricalVariable",
    function (x) callNextMethod(x) %||% categories(entity(x)))
#' @rdname var-categories
#' @export
setMethod("categories", "CategoricalArrayVariable",
    function (x) callNextMethod(x) %||% categories(entity(x)))

#' @rdname var-categories
#' @export
setMethod("categories", "VariableEntity",
    function (x) Categories(data=x@body$categories))

#' @rdname var-categories
#' @export
setMethod("categories<-", c("CategoricalVariable", "Categories"),
    function (x, value) {
        ent <- setEntitySlot(entity(x), "categories", value)
        dropCache(cubeURL(x))
        return(x)
    })
#' @rdname var-categories
#' @export
setMethod("categories<-", c("CategoricalArrayVariable", "Categories"),
    function (x, value) {
        ent <- setEntitySlot(entity(x), "categories", value)
        lapply(subvariables(tuple(x)), dropCache) ## Subvariables will update too
        dropCache(cubeURL(x))
        return(x)
    })
#' @rdname var-categories
#' @export
setMethod("categories<-", c("CategoricalVariable", "numeric"),
    function (x, value) {
        halt("`categories(x) <- value` only accepts Categories, not numeric. ",
            "Did you mean `values(categories(x)) <- value`?")
    })
#' @rdname var-categories
#' @export
setMethod("categories<-", c("CategoricalVariable", "character"),
    function (x, value) {
        halt("`categories(x) <- value` only accepts Categories, not ",
            "character. Did you mean `names(categories(x)) <- value`?")
    })
#' @rdname var-categories
#' @export
setMethod("categories<-", c("CategoricalVariable", "ANY"),
    function (x, value) {
        halt("`categories(x) <- value` only accepts Categories, not ",
            class(value), ".")
    })
#' @rdname var-categories
#' @export
setMethod("categories<-", c("CategoricalArrayVariable", "numeric"),
    function (x, value) {
        halt("`categories(x) <- value` only accepts Categories, not numeric. ",
            "Did you mean `values(categories(x)) <- value`?")
    })
#' @rdname var-categories
#' @export
setMethod("categories<-", c("CategoricalArrayVariable", "character"),
    function (x, value) {
        halt("`categories(x) <- value` only accepts Categories, not ",
            "character. Did you mean `names(categories(x)) <- value`?")
    })
#' @rdname var-categories
#' @export
setMethod("categories<-", c("CategoricalArrayVariable", "ANY"),
    function (x, value) {
        halt("`categories(x) <- value` only accepts Categories, not ",
            class(value), ".")
    })
#' @rdname var-categories
#' @export
setMethod("categories<-", c("CrunchVariable", "ANY"),
    function (x, value) {
        halt("category assignment not defined for ", class(x))
    })

setMethod("datasetReference", "CrunchVariable", function (x) {
    # x@urls$dataset_url
    rootURL(x, "dataset") %||% datasetReference(self(x))
})
setMethod("datasetReference", "character", function (x) {
    # check if the url has /datasets/.*/ in it.
    if (grepl("(.*/datasets/.*?/).*", x)) {
        sub("(.*/datasets/.*?/).*", "\\1", x)
    } else {
        NULL
    }
})
setMethod("datasetReference", "ANY", function (x) NULL)

#' Split an array or multiple-response variable into its CategoricalVariables
#'
#' @param x a `CategoricalArrayVariable` or `MultipleResponseVariable`
#' @return invisibly, the API response from DELETEing the array variable
#' definition. If you [refresh()] the corresponding dataset after
#' unbinding, you should see the array variable removed and its subvariables
#' promoted to regular variables.
#' @export
unbind <- function (x) {
    stopifnot(inherits(x, "CategoricalArrayVariable"))
    ## Delete self and drop cache for variable catalog (parent)
    u <- self(x)
    out <- crPOST(u, body='{"unbind": {}}')
    dropCache(datasetReference(u))
    invisible(out)
}

#' "Subset" a Variable
#'
#' These methods subset variables by creating Expressions, which can be
#' composed and evaluated as needed.
#' @param x a Variable
#' @param i a CrunchExpr, logical, or numeric
#' @param ... additional arguments, ignored
#' @param j Invalid
#' @param drop Invalid
#' @return a CrunchExpr containing references to the variable `x` and the
#' filter logic contained in `i`
#' @aliases variable-extract
#' @name variable-extract
NULL

#' @rdname variable-extract
#' @export
setMethod("[", c("CrunchVariable", "CrunchExpr"), .updateActiveFilter)
#' @rdname variable-extract
#' @export
setMethod("[", c("CrunchVariable", "numeric"), function (x, i, ...) {
    i <- CrunchLogicalExpr(dataset_url=datasetReference(x),
        expression=.dispatchFilter(i))
    return(x[i])
})
#' @rdname variable-extract
#' @export
setMethod("[", c("CrunchVariable", "logical"), .updateActiveFilterLogical)


# for getting and setting the uniform_basis property of multiple response variables.
#' @rdname describe
#' @export
setMethod("uniformBasis", "MultipleResponseVariable", function (x) tuple(x)$uniform_basis)
#' @rdname describe
#' @export
setMethod("uniformBasis<-", "MultipleResponseVariable", function (x, value) {
    stopifnot(is.logical(value))
    # drop cube cache, since this will change the way they are executed
    dropCache(cubeURL(datasetReference(x)))
    return(setTupleSlot(x, "uniform_basis", value))
})
