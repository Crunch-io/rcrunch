init.CategoricalVariable <- function (.Object, ...) {
    .Object <- callNextMethod()
    .Object@body$categories <- Categories(data=.Object@body$categories)
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
    ## For the jsonlite no-simplify deserializer
    if ("subvariables" %in% names(x@body)) {
        x@body[["subvariables"]] <- absoluteURL(unlist(x@body[["subvariables"]]), self(x))
    }
    return(x)
}

##' @rdname refresh
##' @export
setMethod("refresh", "CrunchVariable", function (x) {
    tup <- refresh(tuple(x))
    out <- as.variable(crGET(self(x)), tuple=tup)
    activeFilter(out) <- activeFilter(x)
    return(out)
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

##' @rdname describe
##' @export
setMethod("name", "CrunchVariable", function (x) tuple(x)$name)
##' @rdname describe
##' @export
setMethod("name<-", "CrunchVariable", 
    function (x, value) setTupleSlot(x, "name", value))
##' @rdname describe
##' @export
setMethod("description", "CrunchVariable", function (x) tuple(x)$description)
##' @rdname describe
##' @export
setMethod("description<-", "CrunchVariable", 
    function (x, value) setTupleSlot(x, "description", value))
##' @rdname describe
##' @export
setMethod("alias", "CrunchVariable", function (object) tuple(object)$alias)
##' @rdname describe
##' @export
setMethod("alias<-", "CrunchVariable", 
    function (x, value) setTupleSlot(x, "alias", value))

##' Get and set Categories on Variables
##'
##' @param x a Variable
##' @param value for the setters, an object of class Categories to set. 
##' @return Getters return Categories; setters return \code{x} duly modified.
##' @name var-categories
##' @aliases var-categories categories categories<-
NULL

##' @rdname var-categories
##' @export
setMethod("categories", "CrunchVariable", function (x) NULL)
##' @rdname var-categories
##' @export
setMethod("categories", "CategoricalVariable", function (x) x@body$categories)
##' @rdname var-categories
##' @export
setMethod("categories", "CategoricalArrayVariable",
    function (x) x@body$categories)

##' @rdname var-categories
##' @export
setMethod("categories<-", c("CategoricalVariable", "Categories"), 
    function (x, value) {
        dropCache(absoluteURL("../../cube/", self(x)))
        return(setCrunchSlot(x, "categories", value))
    })
##' @rdname var-categories
##' @export
setMethod("categories<-", c("CategoricalArrayVariable", "Categories"), 
    function (x, value) {
        dropCache(absoluteURL("../../cube/", self(x)))
        lapply(tuple(x)$subvariables, dropCache) ## Subvariables will update too
        return(setCrunchSlot(x, "categories", value))
    })
##' @rdname var-categories
##' @export
setMethod("categories<-", c("CategoricalVariable", "numeric"), 
    function (x, value) {
        halt("`categories(x) <- value` only accepts Categories, not numeric. ",
            "Did you mean `values(categories(x)) <- value`?")
    })
##' @rdname var-categories
##' @export
setMethod("categories<-", c("CategoricalVariable", "character"), 
    function (x, value) {
        halt("`categories(x) <- value` only accepts Categories, not ",
            "character. Did you mean `names(categories(x)) <- value`?")
    })
##' @rdname var-categories
##' @export
setMethod("categories<-", c("CategoricalVariable", "ANY"), 
    function (x, value) {
        halt("`categories(x) <- value` only accepts Categories, not ", 
            class(value), ".")
    })
##' @rdname var-categories
##' @export
setMethod("categories<-", c("CategoricalArrayVariable", "numeric"), 
    function (x, value) {
        halt("`categories(x) <- value` only accepts Categories, not numeric. ",
            "Did you mean `values(categories(x)) <- value`?")
    })
##' @rdname var-categories
##' @export
setMethod("categories<-", c("CategoricalArrayVariable", "character"), 
    function (x, value) {
        halt("`categories(x) <- value` only accepts Categories, not ",
            "character. Did you mean `names(categories(x)) <- value`?")
    })
##' @rdname var-categories
##' @export
setMethod("categories<-", c("CategoricalArrayVariable", "ANY"), 
    function (x, value) {
        halt("`categories(x) <- value` only accepts Categories, not ", 
            class(value), ".")
    })
##' @rdname var-categories
##' @export
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
    ## Delete self and drop cache for variable catalog (parent)
    u <- self(x)
    out <- crDELETE(u)
    dropCache(absoluteURL("../", u))
    invisible(out)
}

##' @rdname delete
##' @export
setMethod("delete", "CategoricalArrayVariable", function (x, ...) {
    u <- self(x)
    subvars <- x@body$subvariables
    out <- crDELETE(u)
    lapply(subvars, crDELETE)
    dropCache(absoluteURL("../", u))
    invisible(out)
})

##' "Subset" a Variable
##'
##' These methods subset variables by creating Expressions, which can be 
##' composed and evaluated as needed.
##' @param x a Variable
##' @param i a CrunchExpr, logical, or numeric
##' @param ... additional arguments, ignored
##' @param j Invalid
##' @param drop Invalid
##' @return a CrunchExpr containing references to the variable \code{x} and the
##' filter logic contained in \code{i}
##' @aliases variable-extract
##' @name variable-extract
NULL

##' @rdname variable-extract
##' @export
setMethod("[", c("CrunchVariable", "CrunchExpr"), function (x, i, ...) {
    CrunchExpr(dataset_url=datasetReference(x), expression=zcl(x),
        filter=zcl(i))
})
##' @rdname variable-extract
##' @export
setMethod("[", c("CrunchVariable", "numeric"), function (x, i, ...) {
    CrunchExpr(dataset_url=datasetReference(x), expression=zcl(x),
        filter=.dispatchFilter(i))
})
##' @rdname variable-extract
##' @export
setMethod("[", c("CrunchVariable", "logical"), function (x, i, ...) {
    CrunchExpr(dataset_url=datasetReference(x), expression=zcl(x),
        filter=.dispatchFilter(i))
})