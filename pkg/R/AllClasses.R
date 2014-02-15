##' Mix-in class for multiple inheritance of variables and datasets.
##' 
##' Exists for common methods in interacting with Crunch API only. Has no
##' Extract methods declared so as not to conflict with the
##' vector/list/data.frame methods jointly inherited in CrunchVariable and
##' CrunchDataset.
ShojiObject <- setClass("ShojiObject",
    representation(
        element="ANY",
        self="ANY",
        description="ANY",
        body="ANY",
        urls="ANY",
        entities="ANY",
        specification="ANY",
        template="ANY"
    ))

CrunchVariable <- setClass("CrunchVariable", contains="ShojiObject",
    representation= representation(
        readonly="logical"
    ), 
    prototype=prototype(readonly=FALSE))
##' @export
NumericVariable <- setClass("NumericVariable", contains="CrunchVariable")
##' @export
CategoricalVariable <- setClass("CategoricalVariable",
    contains="CrunchVariable")
##' @export
TextVariable <- setClass("TextVariable", contains="CrunchVariable")
##' @export
DatetimeVariable <- setClass("DatetimeVariable", contains="CrunchVariable")
##' @export
CategoricalArrayVariable <- setClass("CategoricalArrayVariable", contains="CrunchVariable")
##' @export
MultipleResponseVariable <-setClass("MultipleResponseVariable", contains="CategoricalArrayVariable")

default.useAlias <- function () {
    opt <- getOption("crunch.useAlias")
    return(is.null(opt) || isTRUE(opt))
}

##' @export
CrunchDataset <- setClass("CrunchDataset", contains=c("list", "ShojiObject"),
    representation= representation(
        readonly="logical",
        useAlias="logical",
        .dim="numeric"
    ), 
    prototype=prototype(readonly=FALSE,
        useAlias=default.useAlias(),
        .dim=numeric(2)))

##' @export
Categories <- setClass("Categories", contains="list")
##' @export
Category <- setClass("Category", contains="namedList")

##' @export
VariableGrouping <- setClass("VariableGrouping", contains="list")
##' @export
VariableGroup <- setClass("VariableGroup", representation=representation(
    group="character",
    entities="AsIs"
))
