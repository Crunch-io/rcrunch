##' Mix-in class for multiple inheritance of variables and datasets.
##' 
##' Exists for common methods in interacting with Crunch API only. Has no
##' Extract methods declared so as not to conflict with the
##' vector/list/data.frame methods jointly inherited in CrunchVariable and
##' CrunchDataset.
ShojiObject <- setClass("ShojiObject",
    representation(
        readonly="logical",
        element="ANY",
        self="ANY",
        description="ANY",
        body="ANY",
        urls="ANY",
        catalogs="ANY",
        specification="ANY",
        views="ANY",
        fragments="ANY"
    ),
    prototype=prototype(readonly=FALSE))

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

##' Variables in Crunch
##'
##' Variables are S4 objects. All inherit from the base class
##' \code{CrunchVariable}.
##' @slot readonly logical: should changes made to this variable object locally
##' be persisted on the server? Default is \code{FALSE}
##' @slot tuple An object of class VariableTuple. These contain attributes, such
##' as name and description, that are found in the index of the \code{\link{VariableCatalog}}
CrunchVariable <- setClass("CrunchVariable", contains="ShojiObject",
    representation= representation(
        readonly="logical",
        tuple="VariableTuple"
    ), 
    prototype=prototype(readonly=FALSE, tuple=VariableTuple()))

##' @rdname CrunchVariable
##' @export NumericVariable
NumericVariable <- setClass("NumericVariable", contains="CrunchVariable")

##' @rdname CrunchVariable
##' @export CategoricalVariable
CategoricalVariable <- setClass("CategoricalVariable",
    contains="CrunchVariable")
    
##' @rdname CrunchVariable
##' @export TextVariable
TextVariable <- setClass("TextVariable", contains="CrunchVariable")

##' @rdname CrunchVariable
##' @export DatetimeVariable
DatetimeVariable <- setClass("DatetimeVariable", contains="CrunchVariable")

##' @rdname CrunchVariable
##' @export CategoricalArrayVariable
CategoricalArrayVariable <- setClass("CategoricalArrayVariable",
    contains="CrunchVariable")

##' @rdname CrunchVariable
##' @export MultipleResponseVariable
MultipleResponseVariable <-setClass("MultipleResponseVariable",
    contains="CategoricalArrayVariable")

setClassUnion("characterOrList", c("character", "list"))

##' @export
VariableOrder <- setClass("VariableOrder", contains="list")
##' @export
VariableGroup <- setClass("VariableGroup", representation=representation(
    group="character",
    entities="characterOrList"
))

VariableCatalog <- setClass("VariableCatalog", contains="ShojiCatalog",
    representation(order="VariableOrder"))
DatasetCatalog <- setClass("DatasetCatalog", contains="ShojiCatalog")
BatchCatalog <- setClass("BatchCatalog", contains="ShojiCatalog")

default.useAlias <- function () {
    opt <- getOption("crunch.useAlias")
    return(is.null(opt) || isTRUE(opt))
}

##' @export
CrunchDataset <- setClass("CrunchDataset", contains=c("ShojiObject"),
    representation=representation(
        useAlias="logical",
        .nrow="numeric",
        variables="VariableCatalog",
        tuple="DatasetTuple"
    ), 
    prototype=prototype(
        useAlias=default.useAlias(),
        .nrow=numeric(1),
        variables=VariableCatalog(),
        tuple=DatasetTuple()))

##' @export
Categories <- setClass("Categories", contains="list")
##' @export
Category <- setClass("Category", contains="namedList")

Subvariables <- setClass("Subvariables", contains="ShojiCatalog")

CrunchExpression <- setClass("CrunchExpression",
    representation=representation(
        dataset_url="character",
        expression="list",
        filter="list",
        variables="VariableCatalog"
    ),
    prototype=prototype(
        dataset_url="",
        expression=list(),
        filter=list(),
        variables=VariableCatalog()
    ))
