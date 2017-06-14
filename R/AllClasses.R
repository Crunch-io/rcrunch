#' Mix-in class for multiple inheritance of variables and datasets.
#'
#' Exists for common methods in interacting with Crunch API only. Has no
#' Extract methods declared so as not to conflict with the
#' vector/list/data.frame methods jointly inherited in CrunchVariable and
#' CrunchDataset.
ShojiObject <- setClass("ShojiObject",
    slots=c(
        element="ANY",
        self="ANY",
        body="ANY",
        urls="ANY",
        catalogs="ANY",
        views="ANY",
        fragments="ANY"
    ))
ShojiEntity <- setClass("ShojiEntity", contains="ShojiObject")
ShojiCatalog <- setClass("ShojiCatalog", contains="ShojiObject",
    slots=c(
        index="list",
        orders="list"
    ))
ShojiOrder <- setClass("ShojiOrder", contains="ShojiObject",
    slots=c(
        graph="list",
        catalog_url="character",
        duplicates="logical"
    ),
    prototype=prototype(
        graph=list(),
        catalog_url="",
        duplicates=FALSE
    ))
ShojiView <- setClass("ShojiView", contains="ShojiObject",
    slots=c(
        value="ANY"
    ))

ShojiTuple <- setClass("ShojiTuple",
    slots=c(
        index_url="character",
        entity_url="character",
        body="list"
    ))
VariableTuple <- setClass("VariableTuple", contains="ShojiTuple")
DatasetTuple <- setClass("DatasetTuple", contains="ShojiTuple")
CrunchProject <- setClass("CrunchProject", contains="ShojiTuple")
PermissionTuple <- setClass("PermissionTuple", contains="ShojiTuple")

VariableEntity <- setClass("VariableEntity", contains="ShojiObject")
ProjectEntity <- setClass("ProjectEntity", contains="ShojiObject")
UserEntity <- setClass("UserEntity", contains="ShojiObject")

CrunchExpr <- setClass("CrunchExpr",
    slots=c(
        dataset_url="character",
        expression="list",
        filter="list"
    ),
    prototype=prototype(
        dataset_url="",
        expression=list(),
        filter=list()
    ))

CrunchLogicalExpr <- setClass("CrunchLogicalExpr", contains="CrunchExpr")

#' Variables in Crunch
#'
#' Variables are S4 objects. All inherit from the base class
#' \code{CrunchVariable}.
#' be persisted on the server? Default is \code{FALSE}
#' @slot filter either \code{NULL} or \code{CrunchLogicalExpr}
#' @importFrom methods as callNextMethod new slot slot<- slotNames validObject
#' @rdname CrunchVariable
setClass("CrunchVariable",
     slots=c(
        filter="CrunchLogicalExpr",
        tuple="VariableTuple"
    ),
    prototype=prototype(filter=CrunchLogicalExpr(), tuple=VariableTuple()))

CrunchVariable <- function (tuple, filter=NULL, ...) {
    ## Slight cheat: this isn't the "CrunchVariable" constructor. Instead
    ## returns a subclass of CrunchVariable

    classes <- list(
        categorical="CategoricalVariable",
        numeric="NumericVariable",
        text="TextVariable",
        datetime="DatetimeVariable",
        multiple_response="MultipleResponseVariable",
        categorical_array="CategoricalArrayVariable"
    )
    cls <- classes[[type(tuple)]] %||% "CrunchVariable"
    if (is.null(filter)) {
        filter <- CrunchLogicalExpr()
    }
    return(new(cls, tuple=tuple, filter=filter, ...))
}

#' @rdname CrunchVariable
#' @export NumericVariable
NumericVariable <- setClass("NumericVariable", contains="CrunchVariable")

#' @rdname CrunchVariable
#' @export CategoricalVariable
CategoricalVariable <- setClass("CategoricalVariable",
    contains="CrunchVariable")

#' @rdname CrunchVariable
#' @export TextVariable
TextVariable <- setClass("TextVariable", contains="CrunchVariable")

#' @rdname CrunchVariable
#' @export DatetimeVariable
DatetimeVariable <- setClass("DatetimeVariable", contains="CrunchVariable")

#' @rdname CrunchVariable
#' @export CategoricalArrayVariable
CategoricalArrayVariable <- setClass("CategoricalArrayVariable",
    contains="CrunchVariable")

#' @rdname CrunchVariable
#' @export MultipleResponseVariable
MultipleResponseVariable <-setClass("MultipleResponseVariable",
    contains="CategoricalArrayVariable")

#' Organize Variables within a Dataset
#'
#' Variables in the Crunch web application can be viewed in an ordered,
#' hierarchical list. These objects and methods allow you to modify that order
#' from R.
#'
#' A VariableOrder object is a subclass of \code{list} that contains
#' VariableGroups. VariableGroup objects contain a group name and an set of
#' "entities", which can be variable references or other nested VariableGroups.
#'
#' @slot group character, the name of the VariableGroup. In the constructor and
#' more generally, this field can be referenced as "name" as well.
#' @slot entities a character vector of variable URLs, or a list containing a
#' combination of variable URLs and VariableGroup objects.
#' @slot duplicates logical: should duplicate variable references be allowed in
#' this object? Default is \code{FALSE}.
#' @slot vars either \code{NULL} or a \code{\link{VariableCatalog}}. If not
#' \code{NULL}, it will be used to look up variable names from the URLs.
#' @rdname VariableOrder
#' @export VariableOrder
VariableOrder <- setClass("VariableOrder", contains="ShojiOrder")

OrderGroup <- setClass("OrderGroup",
    slots=c(
        group="character",
        entities="list",
        duplicates="logical"
    ),
    prototype=prototype(
        group="",
        entities=list(),
        duplicates=FALSE
    )
)

#' @rdname VariableOrder
#' @export VariableGroup
VariableGroup <- setClass("VariableGroup", contains="OrderGroup",
    prototype=prototype(
        group="",
        entities=list(),
        duplicates=FALSE
    ))

#' Collection of Variables within a Dataset
#'
#' A VariableCatalog contains references to all variables in a dataset, plus
#' some descriptive metadata about each. VariableCatalogs also contain a
#' \code{\link{VariableOrder}} that governs how variables within it are
#' organized.
#' @rdname VariableCatalog
#' @aliases VariableCatalog
VariableCatalog <- setClass("VariableCatalog", contains="ShojiCatalog",
    slots=c(order="VariableOrder"))
DatasetCatalog <- setClass("DatasetCatalog", contains="ShojiCatalog")
BatchCatalog <- setClass("BatchCatalog", contains="ShojiCatalog")
PermissionCatalog <- setClass("PermissionCatalog", contains="ShojiCatalog")
UserCatalog <- setClass("UserCatalog", contains="ShojiCatalog")
TeamCatalog <- setClass("TeamCatalog", contains="ShojiCatalog")
ProjectCatalog <- setClass("ProjectCatalog", contains="ShojiCatalog")
MemberCatalog <- setClass("MemberCatalog", contains="ShojiCatalog")
VersionCatalog <- setClass("VersionCatalog", contains="ShojiCatalog")
FilterCatalog <- setClass("FilterCatalog", contains="ShojiCatalog")
ForkCatalog <- setClass("ForkCatalog", contains="ShojiCatalog")
MultitableCatalog <- setClass("MultitableCatalog", contains="ShojiCatalog")

#' Crunch Datasets
#'
#' @rdname CrunchDataset
#' @export CrunchDataset
CrunchDataset <- setClass("CrunchDataset", contains=c("ShojiObject"),
    slots=c(
        variables="VariableCatalog",
        filter="CrunchLogicalExpr",
        tuple="DatasetTuple"
    ),
    prototype=prototype(
        variables=VariableCatalog(),
        filter=CrunchLogicalExpr(),
        tuple=DatasetTuple()))

#' Categories in CategoricalVariables
#'
#' CategoricalVariables, as well as the array types composed from
#' Categoricals, contain Categories. Categories are a subclass of list that
#' contains only Category objects. Category objects themselves subclass list
#' and contain the following fields: "name", "id", "numeric_value", "missing",
#' and optionally "selected".
#'
#' @param data For the constructor functions \code{Category} and
#' \code{Categories}, you can either pass in attributes via \code{...} or you
#' can create the objects with a fully defined \code{list} representation of
#' the objects via the \code{data} argument. See the examples.
#' @param x For the attribute getters and setters, an object of class
#' Category or Categories
#' @param i For the [ methods, just as with list extract methods
#' @param j Invalid argument to [, but in the generic's signature
#' @param ... additional arguments to [, ignored
#' @param drop Invalid argument to [, but in the generic's signature
#' @param value For [<-, the replacement Category to insert
#' @rdname Categories
#' @aliases Categories ids ids<- values values<-
#' @export
#' @examples
#' cat.a <- Category(name="First", id=1, numeric_value=1, missing=FALSE)
#' cat.b <- Category(data=list(name="First", id=1, numeric_value=1, missing=FALSE))
#' identical(cat.a, cat.b)
#' cat.c <- Category(name="Second", id=2)
#' cats.1 <- Categories(cat.a, cat.c)
#' cats.2 <- Categories(data=list(cat.a, cat.c))
#' identical(cats.1, cats.2)
setClass("Categories", contains="list")

#' @rdname Categories
#' @export
Categories <- function (..., data=NULL) {
    if (!is.null(data)) {
        return(new("Categories", data))
    } else {
        return(new("Categories", list(...)))
    }
}

#' @rdname Categories
#' @export
setClass("Category", contains="namedList")

#' @rdname Categories
#' @export
Category <- function (..., data=NULL) {
    if (!is.null(data)) {
        return(new("Category", data))
    } else {
        return(new("Category", list(...)))
    }
}

#' @rdname Subvariables
#' @export
Subvariables <- setClass("Subvariables", contains="VariableCatalog",
    slots=c(
        filter="CrunchLogicalExpr"
    ),
    prototype=prototype(
        filter=CrunchLogicalExpr()
    ))

#' @rdname Case
#' @export
Case <- setClass("Case",
                 slots=c(
                     id="integer",
                     name="character",
                     case="CrunchLogicalExpr",
                     numeric_value="numeric",
                     missing="logical"
                 ),
                 prototype=prototype(
                     id=integer(0),
                     case=CrunchLogicalExpr(),
                     missing=FALSE
                 ))

CubeDims <- setClass("CubeDims", contains="namedList",
    slots=c(references="VariableCatalog"),
    prototype=prototype(references=VariableCatalog()))

CrunchCube <- setClass("CrunchCube", contains="list",
    slots=c(
        useNA="character",
        dims="CubeDims",
        arrays="list"),
    prototype=prototype(useNA="no", dims=CubeDims(), arrays=list()))

CrunchTeam <- setClass("CrunchTeam", contains="ShojiObject")
CrunchFilter <- setClass("CrunchFilter", contains="ShojiObject")
Multitable <- setClass("Multitable", contains="ShojiObject")

#' Organize Datsets
#'
#' A DatasetOrder object is a subclass of \code{list} that contains
#' DatasetGroups. DatasetGroup objects contain a group name and an set of
#' "entities", which can be dataset references or other nested DatasetGroups.
#'
#' @slot group character, the name of the DatasetGroup. In the constructor and
#' more generally, this field can be referenced as "name" as well.
#' @slot entities a character vector of dataset URLs, or a list containing a
#' combination of dataset URLs and DatasetGroup objects.
#' @rdname DatasetOrder
#' @export DatasetOrder
DatasetOrder <- setClass("DatasetOrder", contains="ShojiOrder")

#' @rdname DatasetOrder
#' @export DatasetGroup
DatasetGroup <- setClass("DatasetGroup", contains="OrderGroup",
    prototype=prototype(
        group="",
        entities=list(),
        duplicates=FALSE
    ))

setClass("Session", contains="list")

MultitableResult <- setClass("MultitableResult", contains="namedList")
TabBookResult <- setClass("TabBookResult", contains="namedList")

SearchResults <- setClass("SearchResults", contains="namedList")
