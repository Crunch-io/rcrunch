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
        specification="ANY",
        views="ANY",
        entities="ANY" ## prune this
    ))

ShojiCatalog <- setClass("ShojiCatalog", contains="ShojiObject", 
    representation(
        index="list"
    ))

IndexTuple <- setClass("IndexTuple", 
    representation(
        index_url="character",
        entity_url="character",
        body="list"
    ))
VariableTuple <- setClass("VariableTuple", contains="IndexTuple")
DatasetTuple <- setClass("DatasetTuple", contains="IndexTuple")

CrunchVariable <- setClass("CrunchVariable", contains="ShojiObject",
    representation= representation(
        readonly="logical",
        tuple="VariableTuple"
    ), 
    prototype=prototype(readonly=FALSE, tuple=VariableTuple()))

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

##' @export
VariableGrouping <- setClass("VariableGrouping", contains="list")
##' @export
VariableGroup <- setClass("VariableGroup", representation=representation(
    group="character",
    entities="character"
))

VariableCatalog <- setClass("VariableCatalog", contains="ShojiCatalog",
    representation(order="VariableGrouping"))
DatasetCatalog <- setClass("DatasetCatalog", contains="ShojiCatalog")

default.useAlias <- function () {
    opt <- getOption("crunch.useAlias")
    return(is.null(opt) || isTRUE(opt))
}

##' @export
CrunchDataset <- setClass("CrunchDataset", contains=c("ShojiObject"),
    representation=representation(
        readonly="logical",
        useAlias="logical",
        .nrow="numeric",
        variables="VariableCatalog",
        tuple="DatasetTuple"
    ), 
    prototype=prototype(
        readonly=FALSE,
        useAlias=default.useAlias(),
        .nrow=numeric(1),
        variables=VariableCatalog(),
        tuple=DatasetTuple()))

##' @export
Categories <- setClass("Categories", contains="list")
##' @export
Category <- setClass("Category", contains="namedList")


