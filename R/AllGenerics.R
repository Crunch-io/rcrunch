# Well, not exactly _all_ generics. Some are moved into files with their methods
# because pkgdown prefers documentation that way.

#' @export
setGeneric("values", function(x) standardGeneric("values"))
setGeneric("values<-", function(x, value) standardGeneric("values<-"))

setGeneric("id", function(x) standardGeneric("id"))
setGeneric("is.selected", function(x) standardGeneric("is.selected"))
setGeneric("is.selected<-", function(x, value) standardGeneric("is.selected<-"))

setGeneric("ids", function(x) standardGeneric("ids"))
setGeneric("ids<-", function(x, value) standardGeneric("ids<-"))
setGeneric("is.dichotomized", function(x) standardGeneric("is.dichotomized"))
setGeneric("dichotomize", function(x, i) standardGeneric("dichotomize"))
setGeneric("undichotomize", function(x) standardGeneric("undichotomize"))
setGeneric("value", function(x) standardGeneric("value"))
setGeneric("value<-", function(x, value) standardGeneric("value<-"))
setGeneric("name", function(x) standardGeneric("name"))
setGeneric("name<-", function(x, value) standardGeneric("name<-"))
setGeneric("uniformBasis", function(x) standardGeneric("uniformBasis"))
setGeneric("uniformBasis<-", function(x, value) standardGeneric("uniformBasis<-"))
setGeneric("popSize", function(x) standardGeneric("popSize"))
setGeneric("popMagnitude", function(x) standardGeneric("popMagnitude"))
setGeneric("popSize<-", function(x, value) standardGeneric("popSize<-"))
setGeneric("popMagnitude<-", function(x, value) standardGeneric("popMagnitude<-"))
setGeneric("setPopulation", function(x, size, magnitude) standardGeneric("setPopulation"))

setGeneric("expr", function(x) standardGeneric("expr"))
setGeneric("expr<-", function(x, value) standardGeneric("expr<-"),
    signature = "x"
)
setGeneric("description", function(x) standardGeneric("description"))
setGeneric("description<-",
    function(x, value) standardGeneric("description<-"),
    signature = "x"
)
setGeneric("startDate", function(x) standardGeneric("startDate"))
setGeneric("startDate<-",
    function(x, value) standardGeneric("startDate<-"),
    signature = "x"
)
setGeneric("endDate", function(x) standardGeneric("endDate"))
setGeneric("endDate<-",
    function(x, value) standardGeneric("endDate<-"),
    signature = "x"
)
setGeneric("alias")
setGeneric("alias<-", function(x, value) standardGeneric("alias<-"),
    signature = "x"
)
setGeneric("aliases", function(x) standardGeneric("aliases"))
setGeneric("aliases<-", function(x, value) standardGeneric("aliases<-"),
    signature = "x"
)
setGeneric("descriptions", function(x) standardGeneric("descriptions"))
setGeneric("descriptions<-",
    function(x, value) standardGeneric("descriptions<-"),
    signature = "x"
)
setGeneric("emails", function(x) standardGeneric("emails"))

#' Extract the email from a User Entity
#'
#' @param x a UserEntity returned from `me()`
#' @return a character string of the user's email
#' @rdname user-email
setGeneric("email", function(x) standardGeneric("email"))

setGeneric("notes", function(x) standardGeneric("notes"))
setGeneric("notes<-", function(x, value) standardGeneric("notes<-"),
    signature = "x"
)
setGeneric("digits", function(x) standardGeneric("digits"))
setGeneric("digits<-", function(x, value) standardGeneric("digits<-"))
setGeneric("geo", function(x) standardGeneric("geo"))
setGeneric("geo<-", function(x, value) standardGeneric("geo<-"))
setGeneric("fetchGeoFile", function(x) standardGeneric("fetchGeoFile"))
setGeneric("anchor", function(x, ...) standardGeneric("anchor"))
setGeneric("anchors", function(x) standardGeneric("anchors"))
setGeneric("anchor<-", function(x, value) standardGeneric("anchor<-"))
setGeneric("arguments", function(x, ...) standardGeneric("arguments"))
setGeneric("arguments<-", function(x, value) standardGeneric("arguments<-"))
setGeneric("func", function(x) standardGeneric("func"))
setGeneric("funcs", function(x) standardGeneric("funcs"))
setGeneric("subtotals", function(x, ...) standardGeneric("subtotals"))
setGeneric("subtotals<-", function(x, value) standardGeneric("subtotals<-"))
setGeneric("makeInsertion", function(x, var_categories) standardGeneric("makeInsertion"))
setGeneric("subtotalArray", function(x, ...) standardGeneric("subtotalArray"))

setGeneric("types", function(x) standardGeneric("types"))
setGeneric("timestamps", function(x) standardGeneric("timestamps"))
setGeneric("type", function(x) standardGeneric("type"))
setGeneric("type<-", function(x, value) standardGeneric("type<-"))

setGeneric("hide", function(x) standardGeneric("hide"))
setGeneric("unhide", function(x) standardGeneric("unhide"))
setGeneric("derivation", function(x) standardGeneric("derivation"))
setGeneric("derivation<-", function(x, value) standardGeneric("derivation<-"))


setGeneric("urls", function(x) standardGeneric("urls"))
setGeneric("self", function(x) standardGeneric("self"))
setGeneric("refresh", function(x) standardGeneric("refresh"))
setGeneric("entities", function(x, ...) standardGeneric("entities"))
setGeneric("entities<-", function(x, value) standardGeneric("entities<-"))
setGeneric("tuple", function(x) standardGeneric("tuple"))
setGeneric("tuple<-", function(x, value) standardGeneric("tuple<-"))
setGeneric("entity", function(x) standardGeneric("entity"))
setGeneric("index", function(x) standardGeneric("index"))
setGeneric("index<-", function(x, value) standardGeneric("index<-"))
setGeneric("active", function(x) standardGeneric("active"))
setGeneric("hidden", function(x) standardGeneric("hidden"))
setGeneric("archived", function(x) standardGeneric("archived"))
setGeneric("imported", function(x) standardGeneric("imported"))
setGeneric("pending", function(x) standardGeneric("pending"))
setGeneric("permissions", function(x) standardGeneric("permissions"))
setGeneric("members", function(x) standardGeneric("members"))
setGeneric("members<-", function(x, value) standardGeneric("members<-"))

setGeneric("multitables", function(x) standardGeneric("multitables"))
setGeneric("multitables<-", function(x, value) standardGeneric("multitables<-"))
setGeneric("filters", function(x) standardGeneric("filters"))
setGeneric("filters<-", function(x, value) standardGeneric("filters<-"))
setGeneric("appliedFilters", function(x) standardGeneric("appliedFilters"))
setGeneric(
    "appliedFilters<-",
    function(x, value) standardGeneric("appliedFilters<-")
)
setGeneric("activeFilter", function(x) standardGeneric("activeFilter"))
setGeneric(
    "activeFilter<-",
    function(x, value) standardGeneric("activeFilter<-")
)
setGeneric("is.public", function(x) standardGeneric("is.public"))
setGeneric("is.public<-", function(x, value) standardGeneric("is.public<-"))
setGeneric("is.editor", function(x) standardGeneric("is.editor"))
setGeneric("is.editor<-", function(x, value) standardGeneric("is.editor<-"))
setGeneric("is.archived", function(x) standardGeneric("is.archived"))
setGeneric("is.archived<-", function(x, value) standardGeneric("is.archived<-"))
setGeneric("is.draft", function(x) standardGeneric("is.draft"))
setGeneric("is.draft<-", function(x, value) standardGeneric("is.draft<-"))
setGeneric("is.published", function(x) standardGeneric("is.published"))
setGeneric("is.published<-", function(x, value) standardGeneric("is.published<-"))
setGeneric("is.derived", function(x) standardGeneric("is.derived"))
setGeneric("is.derived<-", function(x, value) standardGeneric("is.derived<-"))
setGeneric("as.Text", function(x, ...) standardGeneric("as.Text"))
setGeneric("as.Numeric", function(x) standardGeneric("as.Numeric"))
setGeneric("as.Categorical", function(x, ...) standardGeneric("as.Categorical"))
setGeneric("as.Datetime", function(x,
                                   format = "%Y-%m-%d %H:%M:%S",
                                   resolution,
                                   offset) {
    standardGeneric("as.Datetime")
})
setGeneric("groupClass", function(x) standardGeneric("groupClass"))
setGeneric("entityClass", function(x) standardGeneric("entityClass"))
setGeneric("entitiesInitializer", function(x) standardGeneric("entitiesInitializer"))
setGeneric("folderExtraction", function(x, tuple) standardGeneric("folderExtraction"))
setGeneric("personalFolder", function(x) standardGeneric("personalFolder"))
setGeneric("rootFolder", function(x) standardGeneric("rootFolder"))
setGeneric("weightVariables", function(x) standardGeneric("weightVariables"))
setGeneric("weightVariables<-", function(x, value) standardGeneric("weightVariables<-"))
setGeneric("is.weightVariable<-", function(x, value) standardGeneric("is.weightVariable<-"))
setGeneric("is.weight<-", function(x, value) standardGeneric("is.weight<-"))
setGeneric("whichCatalogEntry", function(x, i, ...) standardGeneric("whichCatalogEntry"))

setGeneric("owner", function(x) standardGeneric("owner"))
setGeneric("owner<-", function(x, value) standardGeneric("owner<-"))

setGeneric("showMissing", function(cube) standardGeneric("showMissing"))
setGeneric("hideMissing", function(cube) standardGeneric("hideMissing"))
setGeneric("showIfAny", function(cube) standardGeneric("showIfAny"))

setGeneric("dim")
setGeneric("ncol")
setGeneric("na.omit")
setGeneric("as.vector")
setGeneric("as.environment")
setGeneric("dimnames")
setGeneric("margin.table")
setGeneric("prop.table")
setGeneric("round")

setGeneric("bases", function(x, margin = NULL) standardGeneric("bases"))
setGeneric("dimensions", function(x) standardGeneric("dimensions"))
setGeneric("dimensions<-", function(x, value) standardGeneric("dimensions<-"))
setGeneric("measures", function(x) standardGeneric("measures"))

setGeneric("subset")
setGeneric("which", signature = "x")

#' Extract and modify Crunch objects
#'
#'
#'
#' @param x a Crunch object (Dataset, Variable, `CrunchExpr`, Catalog,
#' `CrunchCube`, etc.)
#' @param i The elements to extract; as with R, this can generally be (1) a
#' logical vector of length matching `x`; (2) a character vector of appropriate
#' length, which can generally be URLs or also names, aliases, or other
#' identifier; (3) integer indices, potentially negative, to select. Datasets
#' and Variables can also be subset by `CrunchExpr`. Note that when subsetting
#' Datasets and variables by R vectors, the input `i` is turned into a
#' `CrunchExpr` so that it can be used in API queries.
#' @param name for `$`, the same as `i` for `[[`
#' @param j For two-dimensional objects, elements to take from the columnar
#' dimension.
#' @param drop Ignored
#' @param ... additional arguments supported by some methods. For example, some
#' catalogs support a `secondary` vector of elements to match
#' against like `emails(x)` or `owners(x)`; by default this is `names(x)`. This
#' lets (for example) you to look up variables by URL but fall back to name.
#' @param value For updating, an object of the appropriate class and size to
#' insert. This is generally the same class of object you would get from the
#' extract method; i.e. for `x[[i]] <- value`, `value` must be the same class
#' as `x[[i]]`. Exceptions include `ds$var <- VariableDefinition(...)`.
#' @return `[` generally returns a subset of `x`, same class and "self" URL;
#' when subsetting with a `CrunchExpr`, the expression is recorded in an
#' attribute of the object. `[[` and `$`
#' return appropriate extractions from `x`, wherever possible without making an
#' additional API request. The assignment methods return `x` appropriately
#' updated. Most assignment methods do not make an API request except where
#' they clearly are used to create a new entity (as in assigning a variable
#' definition to a Dataset); for editing metadata attributes, these methods
#' generally assume that the API request to set the changes on the server
#' happens in a different method (e.g. in `names(variables(ds)[1:5]) <- value`,
#' assignment happens in the `names<-` method).
#' @name crunch-extract
#' @keywords internal
NULL

#' Generic method for converting objects to Crunch representations
#'
#' R objects are converted to Crunch objects using the following rules:
#'
#' - Character vectors are converted into Crunch text variables
#' - Numeric vectors are converted into Crunch numeric variables
#' - Factors are converted to categorical variables
#' - Date and POSIXt vectors are converted into Crunch datetime variables
#' - Logical vectors are converted to Crunch categorical variables
#' - [VariableDefinition()]s are not converted, but the function can still
#' append additional metadata
#'
#' If you have other object types you wish to convert to Crunch variables,
#' you can declare methods for `toVariable`.
#' @param x An R vector you want to turn into a Crunch variable
#' @param ... Additional metadata fields for the variable, such as "name" and
#' "description". See the [API documentation](http://docs.crunch.io/endpoint-reference/endpoint-variable.html#post-catalog)
#' for a complete list of valid attributes.
#' @return A `VariableDefinition` object. To add this to a dataset, either
#' assign it into the dataset (like `ds$newvar <- toVariable(...)`) or call
#' [addVariables()]. If you're adding a column of data to a dataset, it must be
#' as long as the number of rows in the dataset, or it may be a single value to
#' be recycled for all rows.
#' @rdname toVariable
#' @aliases toVariable
#' @seealso [VariableDefinition()] [addVariables()]
#' @examples
#' var1 <- rnorm(10)
#' toVariable(var1)
#' toVariable(var1, name="Random", description="Generated in R")
#' \dontrun{
#' ds$random <- toVariable(var1, name="Random")
#' # Or, this way:
#' ds <- addVariables(ds, toVariable(var1, name="Random"))
#' }
#' @export
setGeneric("toVariable", function(x, ...) standardGeneric("toVariable"))

setGeneric("lapply")
setGeneric("is.na")
setGeneric("is.na<-")
setGeneric("%in%")
setGeneric("write.csv", function(x, ...) utils::write.csv(x, ...))
setGeneric("duplicated")

setGeneric("zcl", function(x) standardGeneric("zcl"))

#' toJSON methods for Crunch objects
#'
#' `crunch` uses the `jsonlite` package for JSON serialization and
#'  deserialization. Unfortunately, [jsonlite::toJSON()]
#' does not allow for defining S4 methods for other object types. So,
#' `crunch::toJSON` wraps `jsonprep`, which exists to translate
#' objects to base R objects, which `jsonlite::toJSON` can handle.
#' `jsonprep` is defined as an S4 generic, and it is exported, so you can define
#' methods for it if you have other
#' objects that you want to successfully serialize to JSON.
#'
#' @param x the object
#' @param ... additional arguments
#' @return `jsonprep` returns a base R object that `jsonlite::toJSON`
#' can handle. `toJSON` returns the JSON-serialized character object.
#' @name tojson-crunch
#' @seealso [jsonlite::toJSON()]
#' @export
setGeneric("jsonprep", function(x, ...) standardGeneric("jsonprep"))

setGeneric(
    "getShowContent",
    function(x, ...) standardGeneric("getShowContent")
)

.backstopUpdate <- function(x, i, j, value) {
    ## Backstop error so you don't get "Object of class S4 is not subsettable"
    halt(paste("Cannot update", class(x), "with type", class(value)))
}


# for ggplot to not copmlain when given crunchdata
#' Fortify crunch objects for use with ggplot
#'
#' @param model model or other R object to convert to data frame
#' @param data original dataset, if needed
#' @param ... other arguments passed to methods
#' @name fortify
#' @export fortify.CrunchDataFrame
fortify.CrunchDataFrame <- function(model, data, ...) model

#' @rdname fortify
#' @export fortify.CrunchDataset
fortify.CrunchDataset <- function(model, data, ...) model
