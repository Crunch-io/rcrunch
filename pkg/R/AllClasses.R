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
ShojiOrder <- setClass("ShojiOrder", contains="ShojiObject",
    representation(
        value="list"
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
##' @rdname CrunchVariable
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

##' Organize Variables within a Dataset
##'
##' Variables in the Crunch web application can be viewed in an ordered, 
##' hierarchical list. These objects and methods allow you to modify that order
##' from R.
##'
##' A VariableOrder object is a subclass of \code{list} that contains 
##' VariableGroups. VariableGroup objects contain a group name and an set of
##' "entities", which can be variable references or other nested VariableGroups.
##'
##' @slot group character, the name of the VariableGroup. In the constructor and
##' more generally, this field can be referenced as "name" as well.
##' @slot entities a character vector of variable URLs, or a list containing a
##' combination of variable URLs and VariableGroup objects.
##' @rdname VariableOrder
##' @export VariableOrder
VariableOrder <- setClass("VariableOrder", contains="ShojiOrder")

##' @rdname VariableOrder
##' @export VariableGroup
VariableGroup <- setClass("VariableGroup", representation=representation(
    group="character",
    entities="list"
))

##' Collection of Variables within a Dataset
##'
##' A VariableCatalog contains references to all variables in a dataset, plus
##' some descriptive metadata about each. VariableCatalogs also contain a
##' \code{\link{VariableOrder}} that governs how variables within it are
##' organized.
##' @rdname VariableCatalog
VariableCatalog <- setClass("VariableCatalog", contains="ShojiCatalog",
    representation(order="VariableOrder"))
DatasetCatalog <- setClass("DatasetCatalog", contains="ShojiCatalog")
BatchCatalog <- setClass("BatchCatalog", contains="ShojiCatalog")

default.useAlias <- function () {
    opt <- getOption("crunch.useAlias")
    return(is.null(opt) || isTRUE(opt))
}

##' Crunch Datasets
##'
##' @rdname CrunchDataset
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

##' Categories in CategoricalVariables
##' 
##' CategoricalVariables, as well as the array types composed from
##' Categoricals, contain Categories. Categories are a subclass of list that
##' contains only Category objects. Category objects themselves subclass list
##' and contain the following fields: "name", "id", "numeric_value", "missing",
##' and optionally "selected". 
##'
##' @param x For the attribute getters and setters, an object of class
##' Category or Categories
##' @rdname Categories
##' @export
Categories <- setClass("Categories", contains="list")

##' @rdname Categories
##' @export
Category <- setClass("Category", contains="namedList")

##' @rdname Subvariables
##' @export
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

CrunchLogicalExpression <- setClass("CrunchLogicalExpression",
    contains="CrunchExpression")