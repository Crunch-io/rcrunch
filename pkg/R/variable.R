
is.variable <- function (x) inherits(x, "CrunchVariable")
is.numericVariable <- function (x) inherits(x, "NumericVariable")
is.categoricalVariable <- function (x) inherits(x, "CategoricalVariable")
is.textVariable <- function (x) inherits(x, "TextVariable")

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

as.numericVariable <- function (x) as.variable(x, "numeric")
as.categoricalVariable <- function (x) as.variable(x, "categorical")
as.textVariable <- function (x) as.variable(x, "text")

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
