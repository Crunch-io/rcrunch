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
setMethod("alias", "CrunchVariable", function (x) tuple(x)$alias)

##' @export
setMethod("alias<-", "CrunchVariable", 
    function (x, value) setTupleSlot(x, "alias", value))

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
setMethod("datasetReference", "ANY", function (x) NULL)

unbind <- function (x) {
    stopifnot(inherits(x, "CategoricalArrayVariable"))
    invisible(DELETE(self(x)))
}

setMethod("delete", "CategoricalArrayVariable", function (x, ...) {
    subvars <- x@body$subvariables
    out <- DELETE(self(x))
    lapply(subvars, DELETE)
    invisible(out)
})

setMethod("[", c("CrunchVariable", "CrunchExpression"), function (x, i, ...) {
    CrunchExpression(dataset_url=datasetReference(x), expression=zcl(x),
        filter=zcl(i))
})

.updateVariable <- function (variable, value, filter=NULL) {
    payload <- list(command="update", 
        variables=structure(list(zcl(typeof(value, variable))),
            .Names=tuple(variable)$id))
    if (!is.null(filter)) {
        payload[["filter"]] <- zcl(filter)
    }
    update_url <- paste0(datasetReference(variable), "table/")
    # cat(toJSON(payload))
    invisible(POST(update_url, body=toJSON(payload)))
}

setMethod("[<-", c("CrunchVariable", "missing", "missing", "numeric"), 
    function (x, i, j, value) {
        out <- .updateVariable(x, value)
        return(x)
    })

setMethod("[<-", c("CrunchVariable", "numeric", "missing", "numeric"), 
    function (x, i, j, value) {
        ## How to turn numeric into filter?
        # payload[["filter"]] <- list(`function`="in", args=list(list(column=I(seq_len(NUMBEROFROWSINTHEDATASET)), type=list(class="numeric")), list(column=I(i), type=list(class="numeric"))))
        fil <- NULL
        out <- .updateVariable(x, value, filter=fil)
        return(x)
    })

setMethod("[<-", c("CrunchVariable", "CrunchExpression", "missing", "numeric"),
    function (x, i, j, value) {
        ## Need to make sure expression is logical for filtering...
        out <- .updateVariable(x, value, filter=i)
        return(x)
    })

setMethod("[<-", c("TextVariable", "missing", "missing", "character"), 
    function (x, i, j, value) {
        out <- .updateVariable(x, value)
        return(x)
    })

setMethod("[<-", c("TextVariable", "numeric", "missing", "character"), 
    function (x, i, j, value) {
        ## How to turn numeric into filter?
        # payload[["filter"]] <- list(`function`="in", args=list(list(column=I(seq_len(NUMBEROFROWSINTHEDATASET)), type=list(class="numeric")), list(column=I(i), type=list(class="numeric"))))
        fil <- NULL
        out <- .updateVariable(x, value, filter=fil)
        return(x)
    })

setMethod("[<-", c("TextVariable", "CrunchExpression", "missing", "character"),
    function (x, i, j, value) {
        ## Need to make sure expression is logical for filtering...
        out <- .updateVariable(x, value, filter=i)
        return(x)
    })