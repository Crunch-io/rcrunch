init.CategoricalVariable <- function (.Object, ...) {
    .Object <- callNextMethod()
    .Object@body$categories <- Categories(.Object@body$categories)
    return(.Object)
}
setMethod("initialize", "CategoricalVariable", init.CategoricalVariable)
setMethod("initialize", "CategoricalArrayVariable", init.CategoricalVariable)

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
is.Multiple <- is.MR <- is.MultipleResponse <- function (x) inherits(x, "MultipleResponseVariable")

##' @rdname crunch-is
##' @export
is.CA <- is.CategoricalArray <- function (x) class(x) %in% "CategoricalArrayVariable" ## so it doesn't return true for MultipleResponse

.cr.variable.shojiObject <- function (x, ...) {
    out <- CrunchVariable(x, ...)
    return(out)
}

setAs("ShojiObject", "CrunchVariable", 
    function (from) .cr.variable.shojiObject(from))
setAs("shoji", "CrunchVariable", 
    function (from) do.call("CrunchVariable", from))
    
as.variable <- function (x, subtype=NULL) {
    x <- as(x, "CrunchVariable")
    if (is.variable(x)) x <- subclassVariable(x, to=subtype)
    return(x)
}

## In case variable type has been changed, need to instantiate off of new type
setMethod("refresh", "CrunchVariable", function (x) as.variable(GET(self(x))))

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

setMethod("name", "CrunchVariable", function (x) x@body$name)
setMethod("name<-", "CrunchVariable", 
    function (x, value) setCrunchSlot(x, "name", value))
setMethod("description", "CrunchVariable", function (x) x@body$description)
setMethod("description<-", "CrunchVariable", 
    function (x, value) setCrunchSlot(x, "description", value))

setMethod("categories", "CrunchVariable", function (x) x@body$categories)
setMethod("categories<-", "CrunchVariable", 
    function (x, value) setCrunchSlot(x, "categories", value))

.dichotomize.var <- function (x, i) {
    categories(x) <- dichotomize(categories(x), i)
    invisible(refresh(x))
}
.undichotomize.var <- function (x) {
    categories(x) <- undichotomize(categories(x))
    invisible(refresh(x))
}
setMethod("dichotomize", "CategoricalVariable", .dichotomize.var)
setMethod("dichotomize", "CategoricalArrayVariable", .dichotomize.var)
setMethod("undichotomize", "CategoricalVariable", .undichotomize.var)
setMethod("undichotomize", "CategoricalArrayVariable", .undichotomize.var)

setMethod("datasetReference", "CrunchVariable", function (x) x@urls$dataset_url)