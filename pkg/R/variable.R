VARIABLE_TYPES <- c("numeric", "text", "categorical")

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
            text=TextVariable
        )
    if (!is.null(x)) x <- constructors[[x]]
    if (is.null(x)) x <- CrunchVariable
    return(x)
}

setGeneric("type", function (x) standardGeneric("type"))
setMethod("type", "CrunchVariable", function (x) x@body$family)
## do type casting as type<-

castVariable <- function (x, to) {
    if (!(to %in% VARIABLE_TYPES)) {
        stop(sQuote(to), " is not a valid Crunch variable type. Valid types ",
            "are ", serialPaste(sQuote(VARIABLE_TYPES)))
    }
    POST(cast_url(x), body=toJSON(list(cast_as=to)))
    invisible(refresh(x))
}

cast_url <- function (x) x@urls$cast_url

setGeneric("type<-", function (x, value) standardGeneric("type<-"))
setMethod("type<-", "CrunchVariable", 
    function (x, value) castVariable(x, value))

setMethod("name", "CrunchVariable", function (x) x@body$name)
setMethod("description", "CrunchVariable", function (x) x@body$description)

setGeneric("categories", function (x) standardGeneric("categories"))
setMethod("categories", "CrunchVariable", function (x) x@body$categories)