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
is.Array <- function (x) inherits(x, "CategoricalArrayVariable")

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

##' @rdname refresh
##' @export
setMethod("refresh", "CrunchVariable", function (x) {
    as.variable(crGET(self(x)), tuple=refresh(tuple(x)))
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
setMethod("alias", "CrunchVariable", function (object) tuple(object)$alias)

##' @export
setMethod("alias<-", "CrunchVariable", 
    function (x, value) setTupleSlot(x, "alias", value))

##' @export
setMethod("categories", "CrunchVariable", function (x) NULL)
setMethod("categories", "CategoricalVariable", function (x) x@body$categories)
setMethod("categories", "CategoricalArrayVariable",
    function (x) x@body$categories)

##' @export
setMethod("categories<-", c("CategoricalVariable", "Categories"), 
    function (x, value) setCrunchSlot(x, "categories", value))
setMethod("categories<-", c("CategoricalArrayVariable", "Categories"), 
    function (x, value) setCrunchSlot(x, "categories", value))
setMethod("categories<-", c("CategoricalVariable", "numeric"), 
    function (x, value) {
        halt("`categories(x) <- value` only accepts Categories, not numeric. ",
            "Did you mean `values(categories(x)) <- value`?")
    })
setMethod("categories<-", c("CategoricalVariable", "character"), 
    function (x, value) {
        halt("`categories(x) <- value` only accepts Categories, not ",
            "character. Did you mean `names(categories(x)) <- value`?")
    })
setMethod("categories<-", c("CategoricalVariable", "ANY"), 
    function (x, value) {
        halt("`categories(x) <- value` only accepts Categories, not ", 
            class(value), ".")
    })
setMethod("categories<-", c("CategoricalArrayVariable", "numeric"), 
    function (x, value) {
        halt("`categories(x) <- value` only accepts Categories, not numeric. ",
            "Did you mean `values(categories(x)) <- value`?")
    })
setMethod("categories<-", c("CategoricalArrayVariable", "character"), 
    function (x, value) {
        halt("`categories(x) <- value` only accepts Categories, not ",
            "character. Did you mean `names(categories(x)) <- value`?")
    })
setMethod("categories<-", c("CategoricalArrayVariable", "ANY"), 
    function (x, value) {
        halt("`categories(x) <- value` only accepts Categories, not ", 
            class(value), ".")
    })
setMethod("categories<-", c("CrunchVariable", "ANY"), 
    function (x, value) {
        halt("category assignment not defined for ", class(x))
    })

setMethod("datasetReference", "CrunchVariable", function (x) x@urls$dataset_url)
setMethod("datasetReference", "ANY", function (x) NULL)

##' Split an array or multiple-response variable into its CategoricalVariables
##'
##' @param x a CategoricalArrayVariable or MultipleResponseVariable
##' @return invisibly, the API response from DELETEing the array variable
##' definition. If you \code{\link{refresh}} the corresponding dataset after
##' unbinding, you should see the array variable removed and its subvariables
##' promoted to regular variables.
##' @export
unbind <- function (x) {
    stopifnot(inherits(x, "CategoricalArrayVariable"))
    invisible(crDELETE(self(x)))
}

setMethod("delete", "CategoricalArrayVariable", function (x, ...) {
    subvars <- x@body$subvariables
    out <- crDELETE(self(x))
    lapply(subvars, crDELETE)
    invisible(out)
})

setMethod("[", c("CrunchVariable", "CrunchExpression"), function (x, i, ...) {
    CrunchExpression(dataset_url=datasetReference(x), expression=zcl(x),
        filter=zcl(i))
})
setMethod("[", c("CrunchVariable", "numeric"), function (x, i, ...) {
    CrunchExpression(dataset_url=datasetReference(x), expression=zcl(x),
        filter=.dispatchFilter(i))
})
setMethod("[", c("CrunchVariable", "logical"), function (x, i, ...) {
    CrunchExpression(dataset_url=datasetReference(x), expression=zcl(x),
        filter=.dispatchFilter(i))
})