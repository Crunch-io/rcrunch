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
#' `CrunchVariable`.
#' @slot filter either `NULL` or `CrunchLogicalExpr`
#' @slot tuple `VariableTuple`
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
#' A VariableOrder object is a subclass of `list` that contains
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
#' some descriptive metadata about each. Each VariableCatalog also contains a
#' [`VariableOrder`] that governs how variables within it are
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
#' @exportClass CrunchDataset
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

#' Abstract categories
#'
#' An abstract class that categories, elements, insertions, etc. fall under
#'
#' @param data For the constructor functions `AbsCat` and
#' `AbsCats`, you can either pass in attributes via `...` or you
#' can create the objects with a fully defined `list` representation of
#' the objects via the `data` argument. See the examples.
#' @param x For the attribute getters and setters, an object of class
#' AbsCat or AbsCats
#' @param i For the `[` methods, just as with list extract methods
#' @param j Invalid argument to `[`, but in the generic's signature
#' @param ... additional arguments to `[`, ignored
#' @param drop Invalid argument to `[`, but in the generic's signature
#' @param value For `[<-`, the replacement AbsCat to insert
#' @rdname abscat
#' @aliases abscat
#' @keywords internal
#' @export
setClass("AbsCats", contains="list")
#' @rdname abscat
#' @export
AbsCats <- function (..., data=NULL) {
    if (!is.null(data)) {
        return(new("AbsCats", data))
    } else {
        return(new("AbsCats", list(...)))
    }
}

#' @rdname abscat
#' @export
setClass("AbsCat", contains="namedList")

#' @rdname abscat
#' @export
AbsCat <- function (..., data=NULL) {
    if (!is.null(data)) {
        return(new("AbsCat", data))
    } else {
        return(new("AbsCat", list(...)))
    }
}



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
#' @param x For the attribute getters and setters, an object of class
#' Category or Categories
#' @param i For the `[` methods, just as with list extract methods
#' @param j Invalid argument to `[`, but in the generic's signature
#' @param ... additional arguments to `[`, ignored
#' @param drop Invalid argument to `[`, but in the generic's signature
#' @param value For `[<-`, the replacement Category to insert
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
setClass("Categories", contains="AbsCats")

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
setClass("Category", contains="AbsCat")

#' @rdname Categories
#' @export
Category <- function (..., data=NULL) {
    if (!is.null(data)) {
        return(new("Category", data))
    } else {
        return(new("Category", list(...)))
    }
}

#' Insert categories in transformations
#'
#' Insertions allow you to insert new categories into a categorical-like
#' response on a variable's [transform](Transforms).
#'
#' @param data For the constructor functions `Insertion` and
#' `Insertions`, you can either pass in attributes via `...` or you
#' can create the objects with a fully defined `list` representation of
#' the objects via the `data` argument. See the examples.
#' @param x For the attribute getters and setters, an object of class
#' Insertion or Insertions
#' @param ... additional arguments to `[`, ignored
#' @param value For `[<-`, the replacement Insertion to insert
#' @rdname Insertions
#' @aliases anchor anchor<- anchors func func<- funcs args args<-
#' @export
setClass("Insertions", contains="AbsCats")

#' @rdname Insertions
#' @export
Insertions <- function (..., data=NULL) {
    if (!is.null(data)) {
        return(new("Insertions", data))
    } else {
        return(new("Insertions", list(...)))
    }
}

#' @rdname Insertions
#' @export
setClass("Insertion", contains="AbsCat")

#' @rdname Insertions
#' @export
Insertion <- function (..., data=NULL) {
    if (!is.null(data)) {
        out <- new("Insertion", data)
    } else {
        out <- new("Insertion", list(...))
    }
    # ensure that args is a list, even if it is a single element
    if (!is.null(out$args) && length(out$args) == 1) {
        out$args <- as.list(out$args)
    }

    return(out)
}

#' Subtotals and headings
#'
#' Subtotals and headings allow you to add subtotals and headings to variables
#' or CrunchCubes. These are especially useful for making aggregates across
#' multiple categories (sometimes referred to as _nets_, _top box_, or
#' _top 2 box_).
#'
#' To see the subtotals or headings set for a variable, use `subtotals(variable)`
#'
#' Subtotals and headings can be added either by passing a list of `Subtotal`s
#' or `Heading`s, or they can be added one at a time by passing `Subtotal` or
#' `Heading` to `subtotals(variable)` alone.
#'
#' Adding subtotals or headings is additive; meaning that subtotals or headings
#' that are already set on the variable are not removed when new subtotals or
#' headings are added. To remove all subtotals and headings, set
#' `subtotals(variable)` to `NULL`.
#'
#' To get an array of just the subtotal rows from a CrunchCube, use the function
#' `subtotalArray(CrunchCube)`.
#'
#' @param data For the constructor functions `Subtotal` and
#' `Subtotal`, you can either pass in attributes via `...` or you
#' can create the objects with a fully defined `list` representation of
#' the objects via the `data` argument. See the examples.
#' @param x either a variable or CrunchCube object to add or get subtotal
#' transforms for
#' @param ... additional arguments to `[`, ignored
#' @param value For `[<-`, the replacement Subtotal to insert
#' @param var the variable to use to make `Insertions` from a `Subtotal` object
#'
#' @examples
#' \dontrun{
#' # given a variable ds$opinion, with categories: Strongly Agree, Somewhat
#' # Agree, Neither Agree nor Disagree, Somewhat Disagree, and Strongly Disagree,
#' # to make two subtotals for Agree and Disagree:
#' subtotals(ds$opinion) <- list(
#'     Subtotal(name = "Agree", categories = c("Strongly Agree", "Somewhat Agree"),
#'              after = "Somewhat Agree"),
#'     Subtotal(name = "Disagree", categories = c("Strongly Disagree", "Somewhat Disagree"),
#'              after = "Strongly Disagree")
#' )
#'
#' # headings can also be added:
#' subtotals(ds$opinion) <- Heading(name = "All opinions", after = 0)
#'
#' # to see the subtotals and headings associated with a variable
#' subtotals(ds$opinion)
#'
#' # to remove all subtotals and headings
#' subtotals(ds$opinion) <- NULL
#' }
#'
#' @rdname SubtotalsHeadings
#' @aliases subtotals subtotals<- makeInsertion
#' @export
setClass("Subtotal", contains="AbsCat")

#' @rdname SubtotalsHeadings
#' @export
Subtotal <- function (..., data=NULL) {
    if (!is.null(data)) {
        return(new("Subtotal", data))
    } else {
        return(new("Subtotal", list(...)))
    }
}


#' @rdname SubtotalsHeadings
#' @export
setClass("Heading", contains="AbsCat")

#' @rdname SubtotalsHeadings
#' @export
Heading <- function (..., data=NULL) {
    if (!is.null(data)) {
        return(new("Heading", data))
    } else {
        return(new("Heading", list(...)))
    }
}

#' Transformations of variable and cube views
#'
#' Transformations allow you to change how a variable or cube is displayed
#' without changing the underlying data.
#'
#' @param data For the constructor function `Transforms` you can either pass in
#' attributes via `...` or you can create the objects with a fully defined
#' `list` representation of the objects via the `data` argument. See the examples.
#' @param ... For the constructor function `Transforms` you can pass
#' in attributes via `...`
#' @param x For the attribute getters and setters, an object of class
#' Transforms
#' @param value For `[<-`, the replacement Transforms to insert
#' @rdname Transforms
#' @aliases transforms transforms<-
#' @export
setClass("Transforms", contains="namedList")

#' @rdname Transforms
#' @export
Transforms <- function (..., data = NULL) {
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


#' @rdname Subvariables
#' @export
Subvariables <- setClass("Subvariables", contains="VariableCatalog",
    slots=c(
        filter="CrunchLogicalExpr"
    ),
    prototype=prototype(
        filter=CrunchLogicalExpr()
    ))

CubeDims <- setClass("CubeDims", contains="namedList")

CrunchCube <- setClass("CrunchCube", contains="list",
    slots=c(
        useNA="character",
        dims="CubeDims",
        arrays="list"),
    prototype=prototype(useNA="no", dims=CubeDims(), arrays=list()))

CrunchTeam <- setClass("CrunchTeam", contains="ShojiObject")
CrunchFilter <- setClass("CrunchFilter", contains="ShojiObject")
Multitable <- setClass("Multitable", contains="ShojiObject")

#' Organize Datasets
#'
#' A DatasetOrder object is a subclass of `list` that contains
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

#' @rdname geo
setClass("CrunchGeography", contains="namedList")
#' @rdname geo
#' @export
CrunchGeography <- function (..., data=NULL) {
    if (is.null(data)) {
        data <- list(...)
    }
    return(new("CrunchGeography", data))
}

#' @rdname geo
#' @export Geodata
Geodata <- setClass("Geodata", contains="ShojiEntity")
GeoCatalog <- setClass("GeoCatalog", contains="ShojiCatalog")
