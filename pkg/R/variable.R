init.CategoricalVariable <- function (.Object, ...) {
    .Object <- callNextMethod()
    .Object@body$categories <- Categories(.Object@body$categories)
    return(.Object)
}
setMethod("initialize", "CategoricalVariable", init.CategoricalVariable)
setMethod("initialize", "CategoricalArrayVariable", init.CategoricalVariable)

setMethod("tuple", "CrunchVariable", function (x) x@tuple)
setMethod("tuple<-", "CrunchVariable", function (x, value) {
    x@tuple <- value
    return(x)
})

##' @rdname crunch-is
##' @export
is.variable <- function (x) inherits(x, "CrunchVariable")

##' @rdname crunch-is
##' @export
is.Numeric <- function (x) inherits(x, "NumericVariable")

##' @rdname crunch-is
##' @export
is.Categorical <- function (x) inherits(x, "CategoricalVariable")

##' @rdname crunch-is
##' @export
is.Text <- function (x) inherits(x, "TextVariable")

##' @rdname crunch-is
##' @export
is.Datetime <- function (x) inherits(x, "DatetimeVariable")

##' @rdname crunch-is
##' @export
is.Multiple <- function (x) inherits(x, "MultipleResponseVariable")

##' @rdname crunch-is
##' @export
is.MR <- is.Multiple

##' @rdname crunch-is
##' @export
is.MultipleResponse <- is.Multiple

##' @rdname crunch-is
##' @export
is.CA <- function (x) class(x) %in% "CategoricalArrayVariable" ## so it doesn't return true for MultipleResponse

##' @rdname crunch-is
##' @export
is.CategoricalArray <- is.CA
    
as.variable <- function (x, subtype=NULL, tuple=VariableTuple()) {
    x <- CrunchVariable(x)
    if (is.variable(x)) {
        x <- subclassVariable(x, to=subtype)
        tuple(x) <- tuple
    }
    return(x)
}

## In case variable type has been changed, need to instantiate off of new type
##' @export
setMethod("refresh", "CrunchVariable", function (x) {
    as.variable(GET(self(x)), tuple=refresh(tuple(x)))
})

as.Numeric <- function (x) as.variable(x, "numeric")
as.Categorical <- function (x) as.variable(x, "categorical")
as.Text <- function (x) as.variable(x, "text")

subclassVariable <- function (x, to=NULL) {
    if (is.null(to)) to <- type(x)
    Constructor <- pickSubclassConstructor(to)
    return(Constructor(x))
}

pickSubclassConstructor <- function (x=NULL) {
    constructors <- list(
            categorical=CategoricalVariable,
            numeric=NumericVariable,
            text=TextVariable,
            datetime=DatetimeVariable,
            multiple_response=MultipleResponseVariable,
            categorical_array=CategoricalArrayVariable
        )
    if (!is.null(x)) x <- constructors[[x]]
    if (is.null(x)) x <- CrunchVariable
    return(x)
}

##' @export
setMethod("name", "CrunchVariable", function (x) tuple(x)$name)
##' @export
setMethod("name<-", "CrunchVariable", 
    function (x, value) setTupleSlot(x, "name", value))
##' @export
setMethod("description", "CrunchVariable", function (x) tuple(x)$description)
##' @export
setMethod("description<-", "CrunchVariable", 
    function (x, value) setTupleSlot(x, "description", value))

##' @export
setMethod("categories", "CrunchVariable", function (x) NULL)
setMethod("categories", "CategoricalVariable", function (x) x@body$categories)
setMethod("categories", "CategoricalArrayVariable",
    function (x) x@body$categories)

##' @export
setMethod("categories<-", "CategoricalVariable", 
    function (x, value) setCrunchSlot(x, "categories", value))
setMethod("categories<-", "CategoricalArrayVariable", 
    function (x, value) setCrunchSlot(x, "categories", value))

.dichotomize.var <- function (x, i) {
    categories(x) <- dichotomize(categories(x), i)
    invisible(refresh(x))
}
.undichotomize.var <- function (x) {
    categories(x) <- undichotomize(categories(x))
    invisible(refresh(x))
}
##' @export
setMethod("dichotomize", "CategoricalVariable", .dichotomize.var)
##' @export
setMethod("dichotomize", "CategoricalArrayVariable", .dichotomize.var)
##' @export
setMethod("undichotomize", "CategoricalVariable", .undichotomize.var)
##' @export
setMethod("undichotomize", "CategoricalArrayVariable", .undichotomize.var)

setMethod("datasetReference", "CrunchVariable", function (x) x@urls$dataset_url)

unbind <- function (x) {
    stopifnot(inherits(x, "CategoricalArrayVariable"))
    invisible(DELETE(self(x)))
}

setMethod("delete", "CategoricalArrayVariable", function (x) {
    subvars <- x@body$subvariables
    out <- DELETE(self(x))
    lapply(subvars, DELETE)
    invisible(out)
})
