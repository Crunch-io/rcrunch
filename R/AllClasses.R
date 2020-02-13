#' Mix-in class for multiple inheritance of variables and datasets.
#'
#' Exists for common methods in interacting with Crunch API only. Has no
#' Extract methods declared so as not to conflict with the
#' vector/list/data.frame methods jointly inherited in CrunchVariable and
#' CrunchDataset.
ShojiObject <- setClass("ShojiObject",
    slots = c(
        element = "ANY",
        self = "ANY",
        body = "ANY",
        urls = "ANY",
        catalogs = "ANY",
        views = "ANY",
        fragments = "ANY"
    )
)
ShojiEntity <- setClass("ShojiEntity", contains = "ShojiObject")
ShojiCatalog <- setClass("ShojiCatalog",
    contains = "ShojiObject",
    slots = c(
        index = "list",
        orders = "list",
        graph = "list"
    )
)
ShojiFolder <- setClass("ShojiFolder", contains = "ShojiCatalog")
VariableFolder <- setClass("VariableFolder", contains = "ShojiFolder")
ProjectFolder <- setClass("ProjectFolder", contains = "ShojiFolder")
ShojiOrder <- setClass("ShojiOrder",
    contains = "ShojiObject",
    slots = c(
        graph = "list",
        catalog_url = "character",
        duplicates = "logical"
    ),
    prototype = prototype(
        graph = list(),
        catalog_url = "",
        duplicates = FALSE
    )
)
ShojiView <- setClass("ShojiView",
    contains = "ShojiObject",
    slots = c(
        value = "ANY"
    )
)

ShojiTuple <- setClass("ShojiTuple",
    slots = c(
        index_url = "character",
        entity_url = "character",
        body = "list"
    )
)
VariableTuple <- setClass("VariableTuple", contains = "ShojiTuple")
DatasetTuple <- setClass("DatasetTuple", contains = "ShojiTuple")
PermissionTuple <- setClass("PermissionTuple", contains = "ShojiTuple")

VariableEntity <- setClass("VariableEntity", contains = "ShojiObject")
ProjectEntity <- setClass("ProjectEntity", contains = "ShojiObject")
UserEntity <- setClass("UserEntity", contains = "ShojiObject")

CrunchExpr <- setClass("CrunchExpr",
    slots = c(
        dataset_url = "character",
        expression = "list",
        filter = "list"
    ),
    prototype = prototype(
        dataset_url = "",
        expression = list(),
        filter = list()
    )
)

CrunchLogicalExpr <- setClass("CrunchLogicalExpr", contains = "CrunchExpr")

#' Variables in Crunch
#'
#' Variables are S4 objects. All inherit from the base class
#' `CrunchVariable`.
#' @slot filter either `NULL` or `CrunchLogicalExpr`
#' @slot tuple `VariableTuple`
#' @importFrom methods as callNextMethod new slot slot<- slotNames validObject
#' @rdname CrunchVariable
setClass("CrunchVariable",
    slots = c(
        filter = "CrunchLogicalExpr",
        tuple = "VariableTuple"
    ),
    prototype = prototype(filter = CrunchLogicalExpr(), tuple = VariableTuple())
)

.variableClasses <- list(
    categorical = "CategoricalVariable",
    numeric = "NumericVariable",
    text = "TextVariable",
    datetime = "DatetimeVariable",
    multiple_response = "MultipleResponseVariable",
    categorical_array = "CategoricalArrayVariable"
)

CrunchVariable <- function(tuple, filter = NULL, ...) {
    ## Slight cheat: this isn't the "CrunchVariable" constructor. Instead
    ## returns a subclass of CrunchVariable

    cls <- .variableClasses[[type(tuple)]] %||% "CrunchVariable"
    if (is.null(filter)) {
        filter <- CrunchLogicalExpr()
    }
    return(new(cls, tuple = tuple, filter = filter, ...))
}

#' @rdname CrunchVariable
#' @export NumericVariable
NumericVariable <- setClass("NumericVariable", contains = "CrunchVariable")

#' @rdname CrunchVariable
#' @export CategoricalVariable
CategoricalVariable <- setClass("CategoricalVariable",
    contains = "CrunchVariable"
)

#' @rdname CrunchVariable
#' @export TextVariable
TextVariable <- setClass("TextVariable", contains = "CrunchVariable")

#' @rdname CrunchVariable
#' @export DatetimeVariable
DatetimeVariable <- setClass("DatetimeVariable", contains = "CrunchVariable")

#' @rdname CrunchVariable
#' @export CategoricalArrayVariable
CategoricalArrayVariable <- setClass("CategoricalArrayVariable",
    contains = "CrunchVariable"
)

#' @rdname CrunchVariable
#' @export MultipleResponseVariable
MultipleResponseVariable <- setClass("MultipleResponseVariable",
    contains = "CategoricalArrayVariable"
)

#' Organize Variables within a Dataset
#'
#' Variables in the Crunch web application can be viewed in an ordered,
#' hierarchical list. These objects and methods allow you to modify that order
#' from R.
#'
#' A VariableOrder object is a subclass of `list` that contains
#' VariableGroups. VariableGroup objects contain a group name and an set of
#' "entities", which can be variable references or other nested VariableGroups.
#'
#' @slot group character, the name of the VariableGroup. In the constructor and
#' more generally, this field can be referenced as "name" as well.
#' @slot entities a character vector of variable URLs, or a list containing a
#' combination of variable URLs and VariableGroup objects.
#' @slot duplicates logical: should duplicate variable references be allowed in
#' this object? Deprecated field: duplicates are never allowed.
#' @slot vars either `NULL` or a [VariableCatalog()]. If not
#' `NULL`, it will be used to look up variable names from the URLs.
#' @rdname VariableOrder
#' @export VariableOrder
VariableOrder <- setClass("VariableOrder", contains = "ShojiOrder")

OrderGroup <- setClass("OrderGroup",
    slots = c(
        group = "character",
        entities = "list",
        duplicates = "logical"
    ),
    prototype = prototype(
        group = "",
        entities = list(),
        duplicates = FALSE
    )
)

#' @rdname VariableOrder
#' @export VariableGroup
VariableGroup <- setClass("VariableGroup",
    contains = "OrderGroup",
    prototype = prototype(
        group = "",
        entities = list(),
        duplicates = FALSE
    )
)

#' Collection of Variables within a Dataset
#'
#' A VariableCatalog contains references to all variables in a dataset, plus
#' some descriptive metadata about each. Each VariableCatalog also contains a
#' [`VariableOrder`] that governs how variables within it are
#' organized.
#' @rdname VariableCatalog
#' @aliases VariableCatalog
VariableCatalog <- setClass("VariableCatalog",
    contains = "ShojiCatalog",
    slots = c(order = "VariableOrder")
)
DatasetCatalog <- setClass("DatasetCatalog", contains = "ShojiCatalog")
BatchCatalog <- setClass("BatchCatalog", contains = "ShojiCatalog")
PermissionCatalog <- setClass("PermissionCatalog", contains = "ShojiCatalog")
UserCatalog <- setClass("UserCatalog", contains = "ShojiCatalog")
TeamCatalog <- setClass("TeamCatalog", contains = "ShojiCatalog")
MemberCatalog <- setClass("MemberCatalog", contains = "ShojiCatalog")
VersionCatalog <- setClass("VersionCatalog", contains = "ShojiCatalog")
FilterCatalog <- setClass("FilterCatalog", contains = "ShojiCatalog")
ForkCatalog <- setClass("ForkCatalog", contains = "ShojiCatalog")
MultitableCatalog <- setClass("MultitableCatalog", contains = "ShojiCatalog")

#' Crunch Datasets
#'
#' @rdname CrunchDataset
#' @export CrunchDataset
#' @exportClass CrunchDataset
CrunchDataset <- setClass("CrunchDataset",
    contains = c("ShojiObject"),
    slots = c(
        variables = "VariableCatalog",
        hiddenVariables = "VariableCatalog",
        filter = "CrunchLogicalExpr",
        tuple = "DatasetTuple"
    ),
    prototype = prototype(
        variables = VariableCatalog(),
        hiddenVariables = VariableCatalog(),
        filter = CrunchLogicalExpr(),
        tuple = DatasetTuple()
    )
)

DeckCatalog <- setClass("DeckCatalog", contains = "ShojiCatalog")
CrunchDeck <- setClass("CrunchDeck", contains = "ShojiObject")
SlideCatalog <- setClass("SlideCatalog", contains = "ShojiCatalog")
CrunchSlide <- setClass("CrunchSlide", contains = "ShojiObject")
AnalysisCatalog <- setClass("AnalysisCatalog", contains = "ShojiCatalog")
Analysis <- setClass("Analysis", contains = "ShojiObject")

GenericConstructor <- function(class) {
    return(function(..., data = NULL) {
        if (!is.null(data)) {
            return(new(class, data))
        } else {
            return(new(class, list(...)))
        }
    })
}
#' Abstract categories
#'
#' An abstract class that categories, elements, insertions, etc. fall under
#'
#' @param data For the constructor functions `AbstractCategory` and
#' `AbstractCategories`, you can either pass in attributes via `...` or you
#' can create the objects with a fully defined `list` representation of
#' the objects via the `data` argument. See the examples.
#' @param x For the attribute getters and setters, an object of class
#' AbstractCategory or AbstractCategories
#' @param value For `[<-`, the replacement AbstractCategory to insert
#' @rdname AbstractCategory
#' @aliases AbstractCategory
#'
#' @importFrom methods coerce as<- coerce<-
#' @keywords internal
#' @export
setClass("AbstractCategories", contains = "list")


#' @rdname AbstractCategory
#' @export
AbstractCategories <- GenericConstructor("AbstractCategories")

#' @rdname AbstractCategory
#' @export
setClass("AbstractCategory", contains = "namedList")

#' @rdname AbstractCategory
#' @export
AbstractCategory <- GenericConstructor("AbstractCategory")


#' Categories in CategoricalVariables
#'
#' CategoricalVariables, as well as the array types composed from
#' Categoricals, contain Categories. Categories are a subclass of list that
#' contains only Category objects. Category objects are themselves subclasses of
#' lists and contain the following fields: "name", "id", "numeric_value",
#' "missing", and optionally "selected".
#'
#' @param data For the constructor functions `Category` and
#' `Categories`, you can either pass in attributes via `...` or you
#' can create the objects with a fully defined `list` representation of
#' the objects via the `data` argument. See the examples.
#' @param ... Category attributes
#' @rdname Categories
#' @aliases Categories
#' @export
#' @examples
#' cat.a <- Category(name = "First", id = 1, numeric_value = 1, missing = FALSE)
#' cat.b <- Category(data = list(name = "First", id = 1, numeric_value = 1, missing = FALSE))
#' identical(cat.a, cat.b)
#' cat.c <- Category(name = "Second", id = 2)
#' cats.1 <- Categories(cat.a, cat.c)
#' cats.2 <- Categories(data = list(cat.a, cat.c))
#' identical(cats.1, cats.2)
setClass("Categories", contains = "AbstractCategories")

#' @rdname Categories
#' @export
Categories <- GenericConstructor("Categories")

#' @rdname Categories
#' @export
setClass("Category", contains = "AbstractCategory")

#' @rdname Categories
#' @export
Category <- GenericConstructor("Category")


#' @rdname Insertions
#' @export
setClass("Insertions", contains = "AbstractCategories")

#' @rdname Insertions
#' @export
Insertions <- GenericConstructor("Insertions")

#' @rdname Insertions
#' @export
setClass("Insertion", contains = "AbstractCategory")

# Make a constructor with user-facing and package internal versions instead of
# simply setMethod("initialize", "Insertions") so that the user-facing version
# preforms validation, but the internal version does not. This is needed for
# making heterogenous insertions that include classes Subtotal and Heading.
#' @rdname Insertions
#' @export
Insertion <- function(...) {
    out <- .Insertion(...)

    # check validity (only when Insertion is called)
    insertionValidity(out)

    return(out)
}

#' @rdname Insertions
.Insertion <- function(..., data = NULL) {
    out <- GenericConstructor("Insertion")(..., data = data)

    # ensure that args is a list, even if it is a single element
    if (!is.null(out$args) && length(out$args) == 1) {
        out$args <- as.list(out$args)
    }

    return(out)
}


#' @rdname SubtotalsHeadings
#' @export
setClass("Subtotal", contains = "Insertion")

#' @rdname SubtotalsHeadings
#' @export
Subtotal <- GenericConstructor("Subtotal")

#' @rdname SubtotalsHeadings
#' @export
setClass("Heading", contains = "Insertion")

#' @rdname SubtotalsHeadings
#' @export
Heading <- GenericConstructor("Heading")

#' @rdname SummaryStat
#' @export
setClass("SummaryStat", contains = "Insertion")

#' @rdname SummaryStat
#' @export
SummaryStat <- GenericConstructor("SummaryStat")

#' @rdname Transforms
#' @export
setClass("Transforms", contains = "namedList")

#' @rdname Transforms
#' @export
Transforms <- function(..., data = NULL) {
    if (is.null(data)) {
        data <- list(...)
    }

    if (!is.null(data$insertions)) {
        data$insertions <- Insertions(data = data$insertions)
    }
    if (!is.null(data$categories)) {
        data$categories <- Categories(data = data$categories)
    }
    # TODO: add elements
    return(new("Transforms", data))
}

#' @rdname Transforms
#' @export
setClass("TransformsList", contains = "namedList")

#' @rdname Transforms
#' @export
TransformsList <- GenericConstructor("TransformsList")


#' @rdname Subvariables
#' @export
Subvariables <- setClass("Subvariables",
    contains = "VariableCatalog",
    slots = c(
        filter = "CrunchLogicalExpr"
    ),
    prototype = prototype(
        filter = CrunchLogicalExpr()
    )
)

CubeDims <- setClass("CubeDims", contains = "namedList")

CrunchCube <- setClass("CrunchCube",
    contains = "list",
    slots = c(
        useNA = "character",
        dims = "CubeDims",
        arrays = "list"
    ),
    prototype = prototype(useNA = "no", dims = CubeDims(), arrays = list())
)

CrunchTeam <- setClass("CrunchTeam", contains = "ShojiObject")
CrunchFilter <- setClass("CrunchFilter", contains = "ShojiObject")
Multitable <- setClass("Multitable", contains = "ShojiObject")

#' Organize Datasets
#'
#' A DatasetOrder object is a subclass of `list` that contains
#' DatasetGroups. DatasetGroup objects contain a group name and an set of
#' "entities", which can be dataset references or other nested DatasetGroups.
#'
#' This API is deprecated. Use folders instead.
#'
#' @slot group character, the name of the DatasetGroup. In the constructor and
#' more generally, this field can be referenced as "name" as well.
#' @slot entities a character vector of dataset URLs, or a list containing a
#' combination of dataset URLs and DatasetGroup objects.
#' @rdname DatasetOrder
#' @export DatasetOrder
#' @keywords internal
DatasetOrder <- setClass("DatasetOrder", contains = "ShojiOrder")

#' @rdname DatasetOrder
#' @export DatasetGroup
DatasetGroup <- setClass("DatasetGroup",
    contains = "OrderGroup",
    prototype = prototype(
        group = "",
        entities = list(),
        duplicates = FALSE
    )
)

setClass("Session", contains = "list")

MultitableResult <- setClass("MultitableResult", contains = "namedList")
TabBookResult <- setClass("TabBookResult", contains = "namedList")

SearchResults <- setClass("SearchResults", contains = "namedList")

#' @rdname geo
setClass("CrunchGeography", contains = "namedList")
#' @rdname geo
#' @export
CrunchGeography <- GenericConstructor("CrunchGeography")

#' @rdname geo
#' @export Geodata
Geodata <- setClass("Geodata", contains = "ShojiEntity")
GeoCatalog <- setClass("GeoCatalog", contains = "ShojiCatalog")
